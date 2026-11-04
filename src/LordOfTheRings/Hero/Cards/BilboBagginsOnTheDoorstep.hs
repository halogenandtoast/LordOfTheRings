{- | The Baggins sphere Bilbo Baggins of On the Doorstep: his stats, traits and his following
the first player token, for the scenarios that supply him. His game text is the Hobbit
player cards' to write.
-}
module LordOfTheRings.Hero.Cards.BilboBagginsOnTheDoorstep (bilboBagginsOnTheDoorstep) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BilboBagginsOnTheDoorstep = BilboBagginsOnTheDoorstep HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilboBagginsOnTheDoorstep :: HeroCard BilboBagginsOnTheDoorstep
bilboBagginsOnTheDoorstep = hero BilboBagginsOnTheDoorstep Cards.bilboBagginsOnTheDoorstep
