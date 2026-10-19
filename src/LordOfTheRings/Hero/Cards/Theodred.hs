module LordOfTheRings.Hero.Cards.Theodred (theodred) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Theodred = Theodred HeroAttrs
  deriving anyclass (IsHero, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theodred :: HeroCard Theodred
theodred = hero Theodred Cards.theodred
