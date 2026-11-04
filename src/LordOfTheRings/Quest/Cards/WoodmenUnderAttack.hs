module LordOfTheRings.Quest.Cards.WoodmenUnderAttack (woodmenUnderAttack) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype WoodmenUnderAttack = WoodmenUnderAttack QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

woodmenUnderAttack :: QuestCard WoodmenUnderAttack
woodmenUnderAttack = quest WoodmenUnderAttack Cards.woodmenUnderAttack

-- | No more than 1 progress can be placed on each location in the staging area each round.
-- The players cannot defeat this stage unless the first player controls Haldan.
instance HasModifiersFor WoodmenUnderAttack where
  getModifiersFor (WoodmenUnderAttack a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressPerRoundLimit 1]
    haldan <- selectAny (AllyControlledBy FirstPlayer <> AllyWithCardCode (toCardCode Objectives.haldan))
    unless haldan $ modifySelf a [CannotBeDefeated]

-- | Forced: After the players travel to a location, add the top card of the Evil Creatures
-- deck to the staging area.
instance HasAbilities WoodmenUnderAttack where
  getAbilities (WoodmenUnderAttack a) = [mkAbility a 1 $ forced $ AnyTraveledWindow After | a.flipped]

-- | When Revealed: The first player adds Woodmen Village to the staging area. Each other
-- player reveals an encounter card.
instance RunMessage WoodmenUnderAttack where
  runMessage msg q@(WoodmenUnderAttack attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      WoodmenUnderAttack <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.woodmenVillage)
      lead <- getLead
      others <- filter (/= lead) <$> getRemainingPlayers
      for_ others \_ -> revealEncounterCard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      addFromEvilCreaturesDeck attrs
      pure q
    _ -> WoodmenUnderAttack <$> runMessage msg attrs
