module LordOfTheRings.Location.Cards.ForbiddenCoast (forbiddenCoast) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Uncharted, under a Lost Island side.
newtype ForbiddenCoast = ForbiddenCoast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forbiddenCoast :: LocationCard ForbiddenCoast
forbiddenCoast = location ForbiddenCoast Cards.forbiddenCoast

-- | Forced: After Forbidden Coast is flipped over while active, each player may draw a
-- card. Then, each player chooses a card from his hand and places it on the bottom of his
-- deck.
instance HasAbilities ForbiddenCoast where
  getAbilities (ForbiddenCoast a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage ForbiddenCoast where
  runMessage msg l@(ForbiddenCoast a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "forbiddenCoast.draw"
        causedBy a
        labeled "forbiddenCoast.drawCard" $ drawCards a p 1
        labeled "forbiddenCoast.doNotDraw" $ pure ()
      -- the hands as they are once drawn
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        chooseOneM p do
          questionLabeled "forbiddenCoast.bottom"
          causedBy a
          targets hand (putFromHandOnBottomOfDeck p)
      pure l
    _ -> ForbiddenCoast <$> runMessage msg a
