module LordOfTheRings.Treachery.Cards.ShinyDistraction (shinyDistraction) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShinyDistraction = ShinyDistraction TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shinyDistraction :: TreacheryCard ShinyDistraction
shinyDistraction = treachery ShinyDistraction Cards.shinyDistraction

-- | Surge. Doomed 1. Each player chooses independently whether to accept the resources.
instance RunMessage ShinyDistraction where
  runMessage msg t@(ShinyDistraction attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOneM pid do
          questionLabeled "shinyDistraction.choose"
          causedBy attrs
          labeled "shinyDistraction.accept" do
            for_ heroes \h -> placeResources attrs h 1
            encounterRaisesThreat pid 5
          labeled "shinyDistraction.decline" $ pure ()
      pure t
    _ -> ShinyDistraction <$> runMessage msg attrs
