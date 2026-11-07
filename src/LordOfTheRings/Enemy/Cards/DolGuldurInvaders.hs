module LordOfTheRings.Enemy.Cards.DolGuldurInvaders
  (baneOfAmonLanc, sorcererOfDolGuldurAttack, chieftainMorlug, dungeonDweller, hostOfDolGuldur, mirkwoodOlogHai, beastOfTaurNuFuin, amonLancArcher, dolGuldurMarauder, battleWargDolGuldur) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCardCode, toCardDef, cdName, CardType (EnemyType), cdKeywords)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Keyword (Keyword (Archery, Permanent))
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DolGuldurInvader = DolGuldurInvader EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

baneOfAmonLanc, sorcererOfDolGuldurAttack, chieftainMorlug, dungeonDweller, hostOfDolGuldur, mirkwoodOlogHai, beastOfTaurNuFuin, amonLancArcher, dolGuldurMarauder, battleWargDolGuldur :: EnemyCard DolGuldurInvader
baneOfAmonLanc = enemy DolGuldurInvader Cards.baneOfAmonLanc
sorcererOfDolGuldurAttack = enemy DolGuldurInvader Cards.sorcererOfDolGuldurAttack
chieftainMorlug = enemy DolGuldurInvader Cards.chieftainMorlug
dungeonDweller = enemy DolGuldurInvader Cards.dungeonDweller
hostOfDolGuldur = enemy DolGuldurInvader Cards.hostOfDolGuldur
mirkwoodOlogHai = enemy DolGuldurInvader Cards.mirkwoodOlogHai
beastOfTaurNuFuin = enemy DolGuldurInvader Cards.beastOfTaurNuFuin
amonLancArcher = enemy DolGuldurInvader Cards.amonLancArcher
dolGuldurMarauder = enemy DolGuldurInvader Cards.dolGuldurMarauder
battleWargDolGuldur = enemy DolGuldurInvader Cards.battleWargDolGuldur

lieutenants :: [CardDef]
lieutenants = [Cards.baneOfAmonLanc, Cards.sorcererOfDolGuldurAttack, Cards.chieftainMorlug, Cards.dungeonDweller]

instance HasModifiersFor DolGuldurInvader where
  getModifiersFor (DolGuldurInvader a) = do
    let code = toCardCode a
    when (code `elem` map toCardCode lieutenants) $ modifySelf a [ImmuneToPlayerCardEffects]
    when (code == toCardCode Cards.hostOfDolGuldur) do
      players <- length <$> getRemainingPlayers
      modifySelf a [CannotHaveAttachments, ThreatModifier players, GainKeyword $ Archery players]
    when (code == toCardCode Cards.beastOfTaurNuFuin) do
      modifySelf a [CannotHaveAttachments]
      for_ a.shadowCards $ \card -> modified a (CardIdTarget card.id) [ShadowCannotBeCanceled]
    when (code == toCardCode Cards.dolGuldurMarauder) do
      let resources = countTokens Resource $ enemyTokens a
      modifySelf a [AttackModifier resources, DefenseModifier resources]

instance HasAbilities DolGuldurInvader where
  getAbilities (DolGuldurInvader a)
    | code == toCardCode Cards.hostOfDolGuldur =
        [mkAbility a 2 $ forced $ PhaseEndsWindow When EncounterPhase | a.placement == StagingArea]
    | code == toCardCode Cards.mirkwoodOlogHai = [mkAbility a 4 $ forced $ EnemyDestroyedCharacterWithExcessDamageWindow After a.id]
    | code `elem` map toCardCode [Cards.amonLancArcher, Cards.battleWargDolGuldur] =
        [mkAbility a 3 $ forced $ EnemyEngagedWindow After a.id]
    | code `elem` map toCardCode (lieutenants <> [Cards.beastOfTaurNuFuin, Cards.dolGuldurMarauder]) =
        [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]
    | otherwise = []
   where code = toCardCode a

-- FAQ 1.44: an "either" choice must resolve in full when either option can.
progressChoice :: (ReverseQueue m, Sourceable source) => source -> Int -> ChooseT m ()
progressChoice source amount = whenM ((>= amount) <$> getMainQuestProgress) $
  labeledWith "dolGuldur.removeProgress" [intVar "count" amount] $ removeProgressFromMainQuest source amount

-- Encounter effects cannot discard permanent or immune attachments either.
discardableAttachments :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> m [Target]
discardableAttachments source pid = filterM permitted =<< getAttachmentsControlledBy pid
 where
  permitted target = do
    mods <- getModifiers target
    immune <- immuneToSource (toSource source) target
    permanent <- getsGame $ \g -> case target of
      AttachmentTarget aid -> maybe False (member Permanent . cdKeywords . toCardDef) $ Map.lookup aid g.entities.attachments
      _ -> False
    pure $ not immune && not permanent && CannotLeavePlay `notElem` mods && CannotBeDiscardedByEncounterCards `notElem` mods

instance RunMessage DolGuldurInvader where
  runMessage msg e@(DolGuldurInvader a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window When (EnemyAttacking eid pid) <- windows, eid == a.id] $ \pid ->
        if
          | toCardCode a == toCardCode Cards.baneOfAmonLanc -> discardRandomFromHandWithResult a pid 1
          | toCardCode a == toCardCode Cards.sorcererOfDolGuldurAttack -> do
              top <- take 1 . (.deck) <$> getPlayer pid
              discardTopOfDeck pid 1
              for_ top $ \card -> do
                copies <- getsGame $ \g ->
                  [toTarget actor | actor <- toList g.entities.allies, cdName (toCardDef actor) == cdName (toCardDef card)]
                  <> [toTarget actor | actor <- toList g.entities.attachments, cdName (toCardDef actor) == cdName (toCardDef card)]
                  <> [toTarget actor | actor <- toList g.entities.heroes, cdName (toCardDef actor) == cdName (toCardDef card)]
                traverse_ (discard a) copies
          | toCardCode a == toCardCode Cards.chieftainMorlug -> discardEncounterCardsUntilFor a pid (CardWithType EnemyType)
          | toCardCode a == toCardCode Cards.dungeonDweller -> do
              allies <- filterM (fmap not . immuneToSource (toSource a)) =<< select (AllyControlledBy $ PlayerWithId pid)
              chooseOrRunOneM pid do
                questionLabeled "dolGuldur.discardAlly"
                causedBy a
                targets allies (discard a)
          | toCardCode a == toCardCode Cards.beastOfTaurNuFuin -> do
              deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
              mods <- getModifiers a
              let shadows = fromMaybe 1 $ maximumMay [amount | ShadowCardsDealt amount <- mods]
              if null deck || shadows <= 0
                then removeProgressFromMainQuest a 2
                else chooseOrRunOneM pid do
                  questionLabeled "dolGuldur.beast"
                  causedBy a
                  progressChoice a 2
                  labeled "dolGuldur.additionalShadow" $ dealShadowCard a.id
          | otherwise -> chooseOrRunOneM pid do
              questionLabeled "dolGuldur.marauder"
              causedBy a
              progressChoice a 1
              labeled "dolGuldur.addResource" $ placeResources a a 1
      pure e
    DiscardedRandomFromHand (isSource a -> True) _ cards -> do
      enemyGetsAttack a a.id $ sum $ map printedCostOf cards
      pure e
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ (take 1 $ filter (cardMatches $ CardWithType EnemyType) cards) $ \card -> do
        removeFromEncounterDeck card
        putEnemyIntoPlayEngagedFromDiscard pid card
      pure e
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      when (a.placement == StagingArea) do
        players <- length <$> getRemainingPlayers
        removeProgressFromMainQuest a players
      pure e
    UseCardAbility _ (isSource a -> True) 3 windows -> do
      for_ [pid | Window After (EnemyEngaged eid pid) <- windows, eid == a.id] $ \pid ->
        if toCardCode a == toCardCode Cards.amonLancArcher
          then do
            allies <- filterM (\aid -> andM [not <$> immuneToSource (toSource a) (toTarget aid), notElem CannotTakeDamage <$> getModifiers aid]) =<< select (AllyControlledBy $ PlayerWithId pid)
            if null allies
              then removeProgressFromMainQuest a 2
              else chooseOrRunOneM pid do
                questionLabeled "dolGuldur.archer"
                causedBy a
                progressChoice a 2
                targets allies $ \aid -> dealDamage a aid 2
          else do
            attachments <- discardableAttachments a pid
            if null attachments
              then removeProgressFromMainQuest a 1
              else chooseOrRunOneM pid do
                questionLabeled "dolGuldur.battleWarg"
                causedBy a
                progressChoice a 1
                targets attachments (discard a)
      pure e
    UseCardAbility _ (isSource a -> True) 4 _ -> do
      push $ IfAttackDestroyed a.id [DoStep 1 msg]
      pure e
    DoStep 1 (UseCardAbility _ (isSource a -> True) 4 windows) -> do
      for_ [amount | Window After (AttackDealtExcessDamage eid _ amount) <- windows, eid == a.id] $ removeProgressFromMainQuest a
      pure e
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      if
        | toCardCode a == toCardCode Cards.amonLancArcher -> chooseAndExhaust a pid 1
        | toCardCode a == toCardCode Cards.battleWargDolGuldur -> do
            attachments <- discardableAttachments a pid
            chooseOrRunOneM pid do
              questionLabeled "dolGuldur.discardAttachment"
              causedBy a
              targets attachments (discard a)
        | toCardCode a == toCardCode Cards.mirkwoodOlogHai -> do
            attacker <- getEnemy eid
            mods <- getModifiers eid
            if (toAttrs attacker).placement == StagingArea || CannotEnterStagingArea `elem` mods
              then removeProgressFromMainQuest a 2
              else chooseOrRunOneM pid do
                questionLabeled "dolGuldur.ologShadow"
                causedBy a
                progressChoice a 2
                labeled "dolGuldur.returnToStaging" $ placeEnemy eid StagingArea
        | otherwise -> pure ()
      pure e
    _ -> DolGuldurInvader <$> runMessage msg a
