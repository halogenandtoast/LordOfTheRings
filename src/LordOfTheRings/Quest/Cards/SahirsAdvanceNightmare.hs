module LordOfTheRings.Quest.Cards.SahirsAdvanceNightmare (sahirsAdvanceNightmare) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype SahirsAdvanceNightmare = SahirsAdvanceNightmare QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsAdvanceNightmare :: QuestCard SahirsAdvanceNightmare
sahirsAdvanceNightmare = quest SahirsAdvanceNightmare Cards.sahirsAdvanceNightmare

{- | When Revealed: Add Na'asiyah and Captain Sahír to the staging area, enemy side faceup.
Each player reveals the top card of the encounter deck. Place resources on Na'asiyah and
Captain Sahír equal to the amount of damage on the Dream-chaser. Add 1 resource to each
other Raider enemy in play.
Resources on Na'asiyah are considered to be on Captain Sahír, as well (and can be removed
as if they were on him): "LordOfTheRings.Enemy.Cards.CaptainSahir" reads this stage.
Each Raider enemy revealed by the encounter deck enters play with 1 resource on it.
If both Captain Sahír and Na'asiyah are in the victory display, the players win the game.
-}
instance RunMessage SahirsAdvanceNightmare where
  runMessage msg q@(SahirsAdvanceNightmare attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SahirsAdvanceNightmare <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.naasiyah)
      addSetAsideToStaging (cardIs Enemies.captainSahir)
      eachPlayer $ const revealEncounterCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      dreamChaser <- select (LocationWithCardCode $ toCardCode Locations.dreamChaserLocation)
      damage <- sum <$> traverse (getDamage . LocationTarget) dreamChaser
      elite <- concatMapM (select . EnemyWithCardCode . toCardCode) [Enemies.naasiyah, Enemies.captainSahir]
      when (damage > 0) $ for_ elite \e -> placeResources attrs e damage
      others <- select (EnemyWithTrait Raider)
      for_ (filter (`notElem` elite) others) \e -> placeResources attrs e 1
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [eid | Window When (Revealed (EnemyTarget eid)) <- ws] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) $ placeResources attrs eid 1
      SahirsAdvanceNightmare <$> runMessage msg attrs
    AddToVictoryDisplay _ | attrs.flipped -> do
      checkStage attrs.id
      SahirsAdvanceNightmare <$> runMessage msg attrs
    Destroy _ (EnemyTarget _) | attrs.flipped -> do
      checkStage attrs.id
      SahirsAdvanceNightmare <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id && attrs.flipped -> do
      victory <- getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
      when (all ((`elem` victory) . toCardCode) [Enemies.captainSahir, Enemies.naasiyah]) winGame
      pure q
    _ -> SahirsAdvanceNightmare <$> runMessage msg attrs
