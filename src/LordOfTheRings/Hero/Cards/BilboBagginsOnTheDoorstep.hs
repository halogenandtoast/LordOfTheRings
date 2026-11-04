module LordOfTheRings.Hero.Cards.BilboBagginsOnTheDoorstep (bilboBagginsOnTheDoorstep) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype BilboBagginsOnTheDoorstep = BilboBagginsOnTheDoorstep HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Bilbo Baggins: he follows the first player token, by
-- the keyword on his card def, from the scenario's setup (see 'setUpFellowship'). He does
-- not count against the hero limit: the game puts him into play beyond each deck's heroes.
bilboBagginsOnTheDoorstep :: HeroCard BilboBagginsOnTheDoorstep
bilboBagginsOnTheDoorstep = hero BilboBagginsOnTheDoorstep Cards.bilboBagginsOnTheDoorstep

-- | Bilbo Baggins cannot gain resources from non-treasure cards.
instance HasModifiersFor BilboBagginsOnTheDoorstep where
  getModifiersFor (BilboBagginsOnTheDoorstep a) = modifySelf a [CannotGainResourcesFromNonTreasureCards]

-- | Action: Spend 1 [baggins] resource to search your deck for a treasure card and add it to
-- your hand.
instance HasAbilities BilboBagginsOnTheDoorstep where
  getAbilities (BilboBagginsOnTheDoorstep a) =
    [mkAbility a 1 $ ActionAbility $ SphereResourceCost a.heroOwner Baggins 1]

-- | If Bilbo Baggins leaves play, the players have lost the game.
instance RunMessage BilboBagginsOnTheDoorstep where
  runMessage msg h@(BilboBagginsOnTheDoorstep attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      searchDeck pid attrs maxBound TreasureCard False
      pure h
    _ | leavesPlay attrs msg -> do
      gameOver Lost
      BilboBagginsOnTheDoorstep <$> runMessage msg attrs
    _ -> BilboBagginsOnTheDoorstep <$> runMessage msg attrs
