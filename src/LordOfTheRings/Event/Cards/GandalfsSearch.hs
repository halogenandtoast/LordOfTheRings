module LordOfTheRings.Event.Cards.GandalfsSearch (gandalfsSearch) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype GandalfsSearch = GandalfsSearch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Look at the top X cards of any player's deck, add 1 of those cards to its
-- owner's hand, and return the rest to the top of the deck in any order.
gandalfsSearch :: EventCard GandalfsSearch
gandalfsSearch = event GandalfsSearch Cards.gandalfsSearch

instance RunMessage GandalfsSearch where
  runMessage msg e@(GandalfsSearch attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      lore <- sum . map (.resources) <$> (traverse getHero =<< select (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere Lore))
      choosePlayerM pid attrs \p -> do
        deck <- (.deck) <$> getPlayer p
        chooseOneM pid do
          questionLabeled "chooseX"
          causedBy attrs
          for_ [1 .. min lore (length deck)] \x -> labeledWith "xEquals" [intVar "x" x] do
            spendResources pid Lore x
            let cards = take x deck
            chooseOneM pid do
              questionLabeled "gandalfsSearch.take"
              causedBy attrs
              showing cards
              targets cards \c -> do
                push $ TakeFromDeck p c.id
                returnToTopOfDeckInAnyOrder pid attrs (filter (/= c) cards)
      pure e
    _ -> GandalfsSearch <$> runMessage msg attrs
