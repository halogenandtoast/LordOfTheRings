module LordOfTheRings.Objective.Cards.BelegostLoot
  ( swordOfBelegost, mapOfEredLuin, keysOfBelegost
  , bookOfAulesChildren, dwarvenTorch, blueMountainGem
  ) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjective)

newtype BelegostLoot = BelegostLoot ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

swordOfBelegost, mapOfEredLuin, keysOfBelegost, bookOfAulesChildren, dwarvenTorch, blueMountainGem :: ObjectiveCard BelegostLoot
swordOfBelegost = objective BelegostLoot Cards.swordOfBelegost
mapOfEredLuin = objective BelegostLoot Cards.mapOfEredLuin
keysOfBelegost = objective BelegostLoot Cards.keysOfBelegost
bookOfAulesChildren = objective BelegostLoot Cards.bookOfAulesChildren
dwarvenTorch = objective BelegostLoot Cards.dwarvenTorch
blueMountainGem = objective BelegostLoot Cards.blueMountainGem

data LootKind = Sword | Map | Keys | Book | Torch | Gem
  deriving stock Eq

lootKind :: ObjectiveAttrs -> LootKind
lootKind a
  | toCardCode a == toCardCode Cards.swordOfBelegost = Sword
  | toCardCode a == toCardCode Cards.mapOfEredLuin = Map
  | toCardCode a == toCardCode Cards.keysOfBelegost = Keys
  | toCardCode a == toCardCode Cards.bookOfAulesChildren = Book
  | toCardCode a == toCardCode Cards.dwarvenTorch = Torch
  | otherwise = Gem

instance HasModifiersFor BelegostLoot where
  getModifiersFor (BelegostLoot a) = do
    lootObjective a
    when (lootKind a == Sword) $ case a.placement of
      AttachedTo host@(HeroTarget _) -> modified a host [AttackModifier 4]
      _ -> pure ()

instance HasAbilities BelegostLoot where
  getAbilities (BelegostLoot a) = case a.placement of
    AttachedTo (HeroTarget _) | not a.exhausted ->
      [case lootKind a of
        Sword -> restricted a 1 shadowEffectsCancelable $ ResponseAbility (ShadowEffectWindow When) (exhaustCost a)
        kind -> restricted a 1 (criteria kind) $ ActionAbility (exhaustCost a)]
    _ -> []
   where
    criteria = \case
      Map -> exists (not_ UniqueLocation)
      Keys -> exists HeroCanGainEncounterResources
      Book -> PlayerExists PlayerCanDrawFromEncounterCard
      Torch -> exists (not_ UniqueEnemy)
      Gem -> PlayerExists PlayerCanLowerThreatFromEncounterCard
      Sword -> shadowEffectsCancelable

instance RunMessage BelegostLoot where
  runMessage msg o@(BelegostLoot a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      case lootKind a of
        Sword -> cancelShadowEffects windows
        Map -> do
          locations <- select $ not_ UniqueLocation
          chooseOneM pid do
            questionLabeled "belegostLoot.location"
            causedBy a
            targets locations \lid -> phaseModifier a lid (ThreatModifier (-2))
        Keys -> do
          heroes <- select HeroCanGainEncounterResources
          chooseOneM pid do
            questionLabeled "belegostLoot.hero"
            causedBy a
            targets heroes \hid -> placeResources a hid 1
        Book -> do
          players <- select PlayerCanDrawFromEncounterCard
          chooseOrRunOneM pid do
            questionLabeled "belegostLoot.player"
            causedBy a
            targets players \p -> drawCards a p 1
        Torch -> do
          enemies <- select $ not_ UniqueEnemy
          chooseOneM pid do
            questionLabeled "belegostLoot.enemy"
            causedBy a
            targets enemies \eid -> phaseModifiers a eid [AttackModifier (-1), DefenseModifier (-1)]
        Gem -> do
          players <- select PlayerCanLowerThreatFromEncounterCard
          chooseOrRunOneM pid do
            questionLabeled "belegostLoot.player"
            causedBy a
            targets players \p -> lowerThreatBy a p 1
      pure o
    _ -> BelegostLoot <$> runMessage msg a
