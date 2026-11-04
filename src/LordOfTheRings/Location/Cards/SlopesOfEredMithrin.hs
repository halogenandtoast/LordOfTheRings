module LordOfTheRings.Location.Cards.SlopesOfEredMithrin (slopesOfEredMithrin) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype SlopesOfEredMithrin = SlopesOfEredMithrin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slopesOfEredMithrin :: LocationCard SlopesOfEredMithrin
slopesOfEredMithrin = location SlopesOfEredMithrin Cards.slopesOfEredMithrin

-- | Doomed 1 (on its card def).
-- Response: After the players travel to Slopes of Ered Mithrin, the first player may ready
-- a character he controls.
instance HasAbilities SlopesOfEredMithrin where
  getAbilities (SlopesOfEredMithrin a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

-- | Shadow: If the first player controls Urdug, exhaust him.
instance RunMessage SlopesOfEredMithrin where
  runMessage msg l@(SlopesOfEredMithrin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      exhausted <- selectCharacters (HeroControlledBy (PlayerWithId lead) <> ExhaustedHero) (AllyControlledBy (PlayerWithId lead) <> ExhaustedAlly)
      chooseOneM lead do
        questionLabeled "slopesOfEredMithrin.ready"
        causedBy a
        targets exhausted encounterReadies
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      lead <- getLead
      traverse_ exhaust =<< select (AllyWithCardCode (toCardCode Objectives.urdug) <> AllyControlledBy (PlayerWithId lead))
      pure l
    _ -> SlopesOfEredMithrin <$> runMessage msg a
