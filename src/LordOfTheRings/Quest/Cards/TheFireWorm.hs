module LordOfTheRings.Quest.Cards.TheFireWorm (theFireWorm) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (TreacheryType), toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Dragon))

newtype TheFireWorm = TheFireWorm QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFireWorm :: QuestCard TheFireWorm
theFireWorm = quest TheFireWorm Cards.theFireWorm

fireDrakes :: HasGame m => m [EnemyId]
fireDrakes = select (EnemyWithCardCode $ toCardCode Enemies.fireDrake)

-- | Ignore the Deep keyword. This stage cannot be defeated while Fire-drake is in play.
instance HasModifiersFor TheFireWorm where
  getModifiersFor (TheFireWorm a) = when a.flipped do
    modified a GameTarget [IgnoreDeep]
    drakes <- fireDrakes
    unless (null drakes) $ modifySelf a [CannotBeDefeated]

{- | When Revealed: Search the encounter deck and discard pile for a Dragon treachery and
attach it to Fire-drake, if able. Shuffle the encounter deck. The first player chooses
which: one Fire-drake has no copy of already, as Dragon Might's are limit 1 per enemy.

Forced: After a player declares X attackers against Fire-drake, he must remove X progress
tokens from the main quest or cancel the attack. Cancelled, the attackers stay exhausted.
The player is the one who controls the first attacker declared; with fewer than X
progress on the main quest the attack is cancelled.

If Fire-drake is defeated, the players win the game.
-}
instance RunMessage TheFireWorm where
  runMessage msg q@(TheFireWorm attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheFireWorm <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      drakes <- fireDrakes
      lead <- getLead
      for_ (take 1 drakes) \drake -> do
        let dragon = cardMatches (CardWithType TreacheryType <> CardWithTrait Dragon)
        found <-
          filterM (fmap null . (`treacheriesOn` toTarget drake) . toCardCode)
            $ filter dragon (scenario.encounterDeck <> scenario.encounterDiscard)
        chooseOrRunOneM lead do
          questionLabeled "theFireWorm.attach"
          causedBy attrs
          showing (map EncounterCard found)
          targets (map (CardIdTarget . (.id)) found) \t ->
            for_ (find ((== t) . CardIdTarget . (.id)) found) (`attachFromEncounterDiscard` drake)
      shuffleEncounterDeck
      pure q
    CheckWindows ws | attrs.flipped -> do
      drakes <- fireDrakes
      for_ [eid | Window After (AttackersDeclared eid) <- ws, eid `elem` drakes] \_ -> do
        attackers <- selectCharacters AttackingHero AttackingAlly
        controller <- join <$> traverse characterController (headMay attackers)
        pid <- maybe getLead pure controller
        chooseOrRunOneM pid do
          questionLabeled "theFireWorm.attackers"
          causedBy attrs
          removeMainQuestProgressLabeled attrs (length attackers)
          labeled "theFireWorm.cancel" $ for_ attackers stopAttacking
      TheFireWorm <$> runMessage msg attrs
    Destroy _ (EnemyTarget eid) -> do
      drakes <- fireDrakes
      when (eid `elem` drakes) winGame
      TheFireWorm <$> runMessage msg attrs
    _ -> TheFireWorm <$> runMessage msg attrs
