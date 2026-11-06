module LordOfTheRings.Quest.Cards.RaceToMinasTirith (raceToMinasTirith) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (EastBank))

newtype RaceToMinasTirith = RaceToMinasTirith QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

raceToMinasTirith :: QuestCard RaceToMinasTirith
raceToMinasTirith = quest RaceToMinasTirith Cards.raceToMinasTirith

instance HasModifiersFor RaceToMinasTirith where
  getModifiersFor (RaceToMinasTirith a) = when a.flipped do
    east <- select (LocationWithTrait EastBank)
    for_ east \lid -> modified a lid [CannotBeTraveledTo]

instance RunMessage RaceToMinasTirith where
  runMessage msg q@(RaceToMinasTirith attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RaceToMinasTirith <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.theWitchKingOsgiliath)
      pure q
    _ -> RaceToMinasTirith <$> runMessage msg attrs
