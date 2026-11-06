module LordOfTheRings.Treachery.Cards.BarrowDownsSpells
  ( longGropingArm, frozenByFear, dreadfulSong, imWaitingForYou, chillFog ) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition, Enchantment, Wight))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BarrowDownsSpells = BarrowDownsSpells TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

longGropingArm, frozenByFear, dreadfulSong, imWaitingForYou, chillFog :: TreacheryCard BarrowDownsSpells
longGropingArm = treachery BarrowDownsSpells Cards.longGropingArm
frozenByFear = treachery BarrowDownsSpells Cards.frozenByFear
dreadfulSong = treachery BarrowDownsSpells Cards.dreadfulSong
imWaitingForYou = treachery BarrowDownsSpells Cards.imWaitingForYou
chillFog = treachery BarrowDownsSpells Cards.chillFog

waitingAttacks :: TreacheryAttrs -> Maybe ([EnemyId], Bool)
waitingAttacks = parseMaybe parseJSON . treacheryMeta

instance HasModifiersFor BarrowDownsSpells where
  getModifiersFor (BarrowDownsSpells a) = when (toCardCode a == toCardCode Cards.dreadfulSong) $ case a.placement of
    AttachedTo host -> do
      modifySelf a [GainTrait Condition]
      modified a host [WillpowerReducedToZero]
    _ -> pure ()

instance HasAbilities BarrowDownsSpells where
  getAbilities (BarrowDownsSpells a) =
    [mkAbility a 1 $ forced RoundEndsWindow | toCardCode a == toCardCode Cards.dreadfulSong, AttachedTo (HeroTarget _) <- [a.placement]]

instance RunMessage BarrowDownsSpells where
  runMessage msg t@(BarrowDownsSpells a) = case msg of
    WhenRevealed (isTarget a -> True) | toCardCode a == toCardCode Cards.chillFog -> do
      discardAllResourcesThen a (DoStep 1 msg)
      pure t
    WithDiscardedResources pools (DoStep 1 (WhenRevealed (isTarget a -> True))) | toCardCode a == toCardCode Cards.chillFog -> do
      eachPlayer \pid -> encounterRaisesThreat pid $ sum [n | (_, Just controller, n) <- pools, controller == pid]
      when (null pools) $ phaseModifier a a (GainKeyword Surge)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId, toCardCode a == toCardCode Cards.chillFog -> do
      withAttackDamage eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId, toCardCode a == toCardCode Cards.chillFog -> do
      amount <- sum . map snd <$> attackDamaged eid
      encounterRaisesThreat pid amount
      pure t

    WhenRevealed (isTarget a -> True) | toCardCode a == toCardCode Cards.imWaitingForYou -> do
      players <- getStagePlayers
      attacks <- fmap concat $ for players \pid -> do
        enemies <- select $ EnemyWithTrait Wight <> EnemyEngagedWith (PlayerWithId pid)
        pure [(pid, eid) | eid <- enemies]
      -- Recheck each attack as it comes up: an earlier attack may remove an
      -- enemy or prevent a later one from attacking.
      for_ attacks \(pid, eid) -> push $ WithTarget (PlayerTarget pid) (WithTarget (EnemyTarget eid) $ DoStep 1 msg)
      doStep 2 msg
      pure $ BarrowDownsSpells a {treacheryMeta = toJSON (map snd attacks, False)}
    WithTarget (PlayerTarget pid) (WithTarget (EnemyTarget eid) (DoStep 1 (WhenRevealed (isTarget a -> True))))
      | toCardCode a == toCardCode Cards.imWaitingForYou -> do
        present <- selectAny $ EnemyWithId eid
        players <- getRemainingPlayers
        mods <- getModifiers eid
        when (present && pid `elem` players && CannotAttack `notElem` mods && CannotAttackPlayer pid `notElem` mods) do
          -- FAQ 1.42: discard previously dealt shadows before the additional attack.
          push $ DiscardShadowCards eid
          immediateAttack pid eid
        pure t
    CheckWindows ws | toCardCode a == toCardCode Cards.imWaitingForYou, Just (enemies, made) <- waitingAttacks a -> do
      -- Like Out of the Mist, a cancelled attack has no completed-attack window.
      let completed = any (\case Window After (EnemyAttacking eid _) -> eid `elem` enemies; _ -> False) ws
      pure $ BarrowDownsSpells a {treacheryMeta = toJSON (enemies, made || completed)}
    DoStep 2 (WhenRevealed (isTarget a -> True)) | toCardCode a == toCardCode Cards.imWaitingForYou -> do
      unless (maybe False snd $ waitingAttacks a) $ phaseModifier a a (GainKeyword Surge)
      pure $ BarrowDownsSpells a {treacheryMeta = Null}
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      if toCardCode a == toCardCode Cards.frozenByFear then roundModifier a GameTarget CannotTriggerActionsOrResponses
      else if toCardCode a == toCardCode Cards.longGropingArm then do
        -- FAQ 1.55: encounter lasting effects recalculate if engagement changes.
        phaseModifier a GameTarget (EngagedEnemiesAddThreat $ EnemyWithTrait Wight <> EnemyEngagedWith (PlayerWithId you))
        engaged <- select $ EnemyEngagedWith (PlayerWithId you)
        when (null engaged) $ searchEncounterDeckAndDiscardToEngage you a (CardWithType EnemyType <> CardWithTrait Wight)
      else do
        heroes <- filterM eligible =<< select (HeroControlledBy (PlayerWithId you) <> CommittedHero <> HeroCanHaveEncounterAttachment (setFromList [Enchantment, Condition]))
        chooseOrRunOneM you do
          questionLabeled "fog.dreadfulSongHero"
          causedBy a
          targets heroes (attachTreachery a.id)
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        pid <- (.owner) <$> getHero hid
        amount <- length <$> select (EnemyWithTrait Wight <> EnemyEngagedWith (PlayerWithId pid))
        encounterRaisesThreat pid amount
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId, toCardCode a == toCardCode Cards.imWaitingForYou -> do
      let ally = any (\case AllyTarget _ -> True; _ -> False) defenders
      enemyAttackModifier a eid $ AttackModifier $ if ally then 3 else 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId, toCardCode a == toCardCode Cards.frozenByFear -> do
      roundModifier a eid CannotTakeDamage
      pure t
    _ -> BarrowDownsSpells <$> runMessage msg a
   where
    eligible hid = do
      copies <- treacheriesOn (toCardCode a) (HeroTarget hid)
      mods <- getModifiers hid
      pure $ null copies && CannotHaveTreacheryAttachments `notElem` mods
