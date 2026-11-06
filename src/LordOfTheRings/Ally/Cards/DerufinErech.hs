module LordOfTheRings.Ally.Cards.DerufinErech (derufinErech) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype Derufin = Derufin AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

derufinErech :: AllyCard Derufin
derufinErech = ally Derufin Cards.derufin

instance HasModifiersFor Derufin where
  getModifiersFor (Derufin a) = do
    time <- currentNight
    case time of
      Just Dusk -> modifySelf a [WillpowerModifier (-1)]
      Just Midnight -> modifySelf a [WillpowerModifier (-2)]
      _ -> pure ()

instance HasAbilities Derufin where
  getAbilities (Derufin a) =
    [ mkAbility a 1 $ forced $ TraveledToWindow After (LocationWithCardCode $ toCardCode Locations.theStoneOfErechStandalone)
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage Derufin where
  runMessage msg s@(Derufin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      takeControl a.id =<< getLead
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> s <$ gameOver Lost
    _ -> Derufin <$> runMessage msg a
