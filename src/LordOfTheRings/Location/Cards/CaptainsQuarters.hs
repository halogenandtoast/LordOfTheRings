module LordOfTheRings.Location.Cards.CaptainsQuarters (captainsQuarters) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Trait (Trait (Raider))

newtype CaptainsQuarters = CaptainsQuarters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

captainsQuarters :: LocationCard CaptainsQuarters
captainsQuarters = location CaptainsQuarters Cards.captainsQuarters

-- | While Captain's Quarters is grappled, Raider characters are considered to have no
-- resources: the Raider enemies, Captain Sahír's objective-ally and the like included, as
-- the scenario's cards read the resources on its Raider enemies.
instance HasModifiersFor CaptainsQuarters where
  getModifiersFor (CaptainsQuarters a) = do
    whenM (selectAny $ EnemyGrappledWith (LocationWithId a.id)) do
      enemies <- select (EnemyWithTrait Raider)
      allies <- select (AllyWithTrait Raider)
      for_ enemies \e -> modified a e [HasNoResources]
      for_ allies \x -> modified a x [HasNoResources]

-- | Travel: Add 2 resources to Captain Sahír (even if he is out of play) and 1 resource to
-- each other Raider card in play.
instance RunMessage CaptainsQuarters where
  runMessage msg (CaptainsQuarters a) = case msg of
    TravelTo lid | lid == a.id -> do
      let sahirEnemy = EnemyWithCardCode (toCardCode Enemies.captainSahir)
          sahirAlly = AllyWithCardCode (toCardCode Objectives.captainSahirObjective)
      traverse_ (\e -> placeResources a e 2) =<< select sahirEnemy
      traverse_ (\x -> placeResources a x 2) =<< select sahirAlly
      outOfPlay <- getSetAside (cardIs Enemies.captainSahir)
      for_ outOfPlay \card -> placeResourcesOutOfPlay card.id 2
      traverse_ (\e -> placeResources a e 1) =<< select (EnemyWithTrait Raider <> NotEnemy sahirEnemy)
      traverse_ (\x -> placeResources a x 1) =<< select (AllyWithTrait Raider <> NotAlly sahirAlly)
      CaptainsQuarters <$> runMessage msg a
    _ -> CaptainsQuarters <$> runMessage msg a
