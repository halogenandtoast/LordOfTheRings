module LordOfTheRings.Location.Cards.GreatReedThicket (greatReedThicket) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (smeagol, smeagolTravelModifiers, exhaustSmeagolToTravel)

newtype GreatReedThicket = GreatReedThicket LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

greatReedThicket :: LocationCard GreatReedThicket
greatReedThicket = location GreatReedThicket Cards.greatReedThicket

instance HasModifiersFor GreatReedThicket where
  getModifiersFor (GreatReedThicket a) = smeagolTravelModifiers a

instance HasAbilities GreatReedThicket where
  getAbilities (GreatReedThicket a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

instance RunMessage GreatReedThicket where
  runMessage msg l@(GreatReedThicket a) = case msg of
    TravelTo lid | lid == a.id -> do
      exhaustSmeagolToTravel
      GreatReedThicket <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      smeagols <- select smeagol
      gollums <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.gollumTheLandOfShadow)
      chooseOrRunOneM lead do
        questionLabeled "greatReedThicket.choose"
        causedBy a
        when (notNull smeagols) $ labeled "greatReedThicket.flip" $ traverse_ flipOver smeagols
        for_ gollums \g -> labeled "greatReedThicket.attack" do
          let defender = case g.placement of
                EngagedWith pid -> pid
                _ -> lead
          immediateAttack defender g.id
      pure l
    _ -> GreatReedThicket <$> runMessage msg a
