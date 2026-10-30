module LordOfTheRings.Quest.Cards.SahirsAdvance (sahirsAdvance) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype SahirsAdvance = SahirsAdvance QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsAdvance :: QuestCard SahirsAdvance
sahirsAdvance = quest SahirsAdvance Cards.sahirsAdvance

{- | When Revealed: Add Na'asiyah and Captain Sahír to the staging area, enemy side faceup.
Each player reveals the top card of the encounter deck. Place resources on Na'asiyah and
Captain Sahír equal to the amount of damage on The Dream-chaser. Add 1 resource to each
other Raider enemy in play.
Any time players would place progress on this quest, remove an equal amount of damage
from locations in play instead.
Each Raider enemy revealed by the encounter deck enters play with 1 resource on it.
If Captain Sahír is in the victory display and each location in play has no damage, the
players win the game.
-}
instance RunMessage SahirsAdvance where
  runMessage msg q@(SahirsAdvance attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SahirsAdvance <$> runMessage msg attrs
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
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      doStep n msg
      pure q
    -- each point of it removes 1 damage from a location of the first player's choice
    DoStep k (PlaceTokens _ (isTarget attrs -> True) Progress _) | k > 0 -> do
      damaged <- filterM (fmap (> 0) . getDamage . LocationTarget) =<< select AnyLocation
      unless (null damaged) do
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "sahirsAdvance.removeDamage"
          causedBy attrs
          targets damaged \l -> removeTokens attrs l Damage 1
        doStep (k - 1) (dropStep msg)
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [eid | Window When (Revealed (EnemyTarget eid)) <- ws] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) $ placeResources attrs eid 1
      SahirsAdvance <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id && attrs.flipped -> do
      victory <- getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
      damaged <- anyM (fmap (> 0) . getDamage . LocationTarget) =<< select AnyLocation
      when (toCardCode Enemies.captainSahir `elem` victory && not damaged) winGame
      pure q
    _ | attrs.flipped, mayWin -> do
      checkStage attrs.id
      SahirsAdvance <$> runMessage msg attrs
    _ -> SahirsAdvance <$> runMessage msg attrs
   where
    dropStep = \case
      DoStep _ inner -> inner
      m -> m
    -- Captain Sahír going to the victory display, or a location's damage going
    mayWin = case msg of
      Destroy _ (EnemyTarget _) -> True
      AddToVictoryDisplay _ -> True
      Discard (LocationTarget _) -> True
      PlaceUnderneath _ _ -> True
      RemoveTokens _ (LocationTarget _) Damage _ -> True
      _ -> False
