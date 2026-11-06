module LordOfTheRings.Treachery.Cards.BarrowDownsSpells
  ( longGropingArm, frozenByFear, dreadfulSong ) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition, Enchantment, Wight))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BarrowDownsSpells = BarrowDownsSpells TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

longGropingArm, frozenByFear, dreadfulSong :: TreacheryCard BarrowDownsSpells
longGropingArm = treachery BarrowDownsSpells Cards.longGropingArm
frozenByFear = treachery BarrowDownsSpells Cards.frozenByFear
dreadfulSong = treachery BarrowDownsSpells Cards.dreadfulSong

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
    ResolveShadowEffect cid eid _ _ | cid == a.cardId, toCardCode a == toCardCode Cards.frozenByFear -> do
      roundModifier a eid CannotTakeDamage
      pure t
    _ -> BarrowDownsSpells <$> runMessage msg a
   where
    eligible hid = do
      copies <- treacheriesOn (toCardCode a) (HeroTarget hid)
      mods <- getModifiers hid
      pure $ null copies && CannotHaveTreacheryAttachments `notElem` mods
