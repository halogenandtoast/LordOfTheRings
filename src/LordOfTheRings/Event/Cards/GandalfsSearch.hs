module LordOfTheRings.Event.Cards.GandalfsSearch (gandalfsSearch) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype GandalfsSearch = GandalfsSearch EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Look at the top X cards of any player's deck, add 1 of those cards to its
-- owner's hand, and return the rest to the top of the deck in any order.
--
-- ponytail: the rest go back in the order they were in; choosing an order can come with a
-- card that cares about it.
gandalfsSearch :: EventCard GandalfsSearch
gandalfsSearch = event GandalfsSearch Cards.gandalfsSearch

instance RunMessage GandalfsSearch where
  runMessage msg e@(GandalfsSearch attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      lore <- sum . map (.resources) <$> (traverse getHero =<< select (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere Lore))
      order <- getPlayerOrder
      looks <- for order \p -> do
        deck <- (.deck) <$> getPlayer p
        pure
          ( p
          , [ Label (ikeyWith "label.xEquals" [intVar "x" x]) [SpendResources pid Lore x, lookAt p (take x deck)]
            | x <- [1 .. min lore (length deck)]
            ]
          )
      chooseDeck <- choosePlayer pid (Just $ toTarget attrs) \p ->
        [questionLabelWith (ikey "label.chooseX") (Just $ toTarget attrs) [] pid xs | Just xs@(_ : _) <- [lookup p looks]]
      push chooseDeck
      pure e
    _ -> GandalfsSearch <$> runMessage msg attrs
   where
    lookAt p cards =
      Ask (eventOwner attrs)
        $ QuestionLabel (ikey "label.gandalfsSearch.take") (Just $ toTarget attrs) []
        $ SearchCards cards
        $ ChooseOne [targetLabel (CardIdTarget c.id) [TakeFromDeck p c.id] | c <- cards]
