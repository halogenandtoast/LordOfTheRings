module LordOfTheRings.Treachery.Cards.AnnuminasTreacheries
  (theHordesOfAngmar, sorceryOfCarnDum, caughtInThePress, wearyWarriors) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EnemyType), cdKeywords, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Helpers.Character (getHitPoints)
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Keyword (Keyword (Permanent))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AnnuminasTreachery = AnnuminasTreachery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHordesOfAngmar, sorceryOfCarnDum, caughtInThePress, wearyWarriors :: TreacheryCard AnnuminasTreachery
theHordesOfAngmar = treachery AnnuminasTreachery Cards.theHordesOfAngmar
sorceryOfCarnDum = treachery AnnuminasTreachery Cards.sorceryOfCarnDum
caughtInThePress = treachery AnnuminasTreachery Cards.caughtInThePress
wearyWarriors = treachery AnnuminasTreachery Cards.wearyWarriors

instance RunMessage AnnuminasTreachery where
  runMessage msg t@(AnnuminasTreachery a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      -- Resolve each player's choice against the state after earlier players'
      -- choices, including the remaining encounter search supply.
      eachPlayer $ \pid -> push $ WithTarget (PlayerTarget pid) $ DoStep 1 msg
      when (toCardCode a == toCardCode Cards.wearyWarriors) $
        roundModifier a GameTarget (EachDamagedCharacter Blank)
      pure t
    WithTarget (PlayerTarget pid) (DoStep 1 (WhenRevealed (isTarget a -> True))) -> do
      if
        | toCardCode a == toCardCode Cards.theHordesOfAngmar -> do
            engaged <- select $ EnemyEngagedWith $ PlayerWithId pid
            threats <- for engaged $ \eid -> (eid,) <$> getEnemyThreat eid
            let highest = [eid | (eid, n) <- threats, Just n == maximumMay (map snd threats)]
            returning <- filterM (fmap (notElem CannotEnterStagingArea) . getModifiers) highest
            available <- getsGame $ \g -> let scenario = toAttrs g.scenario in scenario.encounterDeck <> scenario.encounterDiscard
            let canSearch = any (cardMatches $ CardWithType EnemyType) available
            chooseOrRunOneM pid do
              questionLabeled "annuminas.hordes"
              causedBy a
              when (notNull returning) $ labeled "annuminas.returnHighest" $ chooseOrRunOneM pid do
                questionLabeled "annuminas.highestEnemy"
                causedBy a
                targets returning $ \eid -> placeEnemy eid StagingArea
              when canSearch $ labeled "annuminas.searchEnemy" $ searchEncounterDeckAndDiscardToEngage pid a (CardWithType EnemyType)
        | toCardCode a == toCardCode Cards.sorceryOfCarnDum -> do
            attachments <- getAttachmentsControlledBy pid
            highest <- filterM (canDiscardAttachment a) =<< highestCostAttachmentsControlledBy pid
            characters <- selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
            capacity <- sum <$> traverse (damageCapacity a) characters
            let n = length attachments
            when (n > 0) $
              if null highest && capacity < n
                then dealDamageDivided a pid n
                else chooseOrRunOneM pid do
                  questionLabeled "annuminas.sorcery"
                  causedBy a
                  when (notNull highest) $ labeled "annuminas.discardHighest" $ chooseOrRunOneM pid do
                    questionLabeled "discardHighestCostAttachment"
                    causedBy a
                    targets highest (discard a)
                  when (capacity >= n) $ labeledWith "annuminas.assignDamage" [intVar "count" n] $ dealDamageDivided a pid n
        | toCardCode a == toCardCode Cards.caughtInThePress -> do
            characters <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId pid)) (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
            damageable <- allM (fmap (> 0) . damageCapacity a) characters
            when (notNull characters) $ chooseOrRunOneM pid do
              questionLabeled "annuminas.caughtInThePress"
              causedBy a
              when damageable $ labeled "annuminas.damageQuesting" $ for_ characters $ \target -> dealDamage a target 1
              labeled "annuminas.reduceWillpower" $ for_ characters $ \target -> phaseModifier a target (WillpowerModifier (-1))
        | otherwise -> do
            heroes <- select $ HeroControlledBy $ PlayerWithId pid
            chooseOrRunOneM pid do
              questionLabeled "annuminas.wearyHero"
              causedBy a
              targets heroes $ \hid -> do
                dealDamage a hid 1
                removeFromQuest hid
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      if
        | toCardCode a == toCardCode Cards.theHordesOfAngmar -> ifThisAttackDestroys eid msg
        | toCardCode a == toCardCode Cards.sorceryOfCarnDum -> do
            attachments <- filterM (canDiscardAttachment a) =<< getAttachmentsControlledBy pid
            chooseOrRunOneM pid do
              questionLabeled "annuminas.discardAttachment"
              causedBy a
              targets attachments (discard a)
        | toCardCode a == toCardCode Cards.caughtInThePress -> do
            progress <- getMainQuestProgress
            attacker <- getEnemy eid
            if (toAttrs attacker).placement == StagingArea
              then removeProgressFromMainQuest a 2
              else chooseOrRunOneM pid do
                questionLabeled "annuminas.pressShadow"
                causedBy a
                when (progress >= 2) $ labeledWith "annuminas.removeProgress" [intVar "count" 2] $ removeProgressFromMainQuest a 2
                labeled "annuminas.returnAfterAttack" $ doStep 1 msg
        | otherwise -> do
            damaged <- anyM (fmap (> 0) . getDamage) defenders
            enemyGetsAttack a eid (if damaged then 2 else 1)
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      if toCardCode a == toCardCode Cards.theHordesOfAngmar
        then revealEncounterCardAs pid
        else returnToStagingAfterAttack eid
      pure t
    _ -> AnnuminasTreachery <$> runMessage msg a

canDiscardAttachment :: HasGame m => TreacheryAttrs -> Target -> m Bool
canDiscardAttachment a target = do
  mods <- getModifiers target
  immune <- immuneToSource (toSource a) target
  permanent <- getsGame $ \g -> case target of
    AttachmentTarget aid -> maybe False (member Permanent . cdKeywords . toCardDef) $ Map.lookup aid g.entities.attachments
    _ -> False
  pure $ not immune && not permanent && CannotLeavePlay `notElem` mods && CannotBeDiscardedByEncounterCards `notElem` mods

damageCapacity :: HasGame m => TreacheryAttrs -> Target -> m Int
damageCapacity a target = do
  mods <- getModifiers target
  immune <- immuneToSource (toSource a) target
  if immune || CannotTakeDamage `elem` mods
    then pure 0
    else max 0 <$> ((-) <$> getHitPoints target <*> getDamage target)
