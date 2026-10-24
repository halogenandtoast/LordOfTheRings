module LordOfTheRings.Event.Cards.AVeryGoodTale (aVeryGoodTale) where

import LordOfTheRings.Card (
  Card (PlayerCard),
  CardCost (StaticCost),
  CardDef (..),
  HasCardDef,
  PlayerCard,
  toCardDef,
 )
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted hiding (first, second)

newtype AVeryGoodTale = AVeryGoodTale EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

{- | Action: Exhaust 2 allies you control to shuffle your deck and discard the top 5
cards. Put up to 2 allies discarded by this effect into play under your control. The
total cost of the allies put into play cannot exceed the total cost of the allies
exhausted to pay for this effect.
-}
aVeryGoodTale :: EventCard AVeryGoodTale
aVeryGoodTale =
  eventWith AVeryGoodTale Cards.aVeryGoodTale \a ->
    a
      & criteriaL
      .~ CharactersAtLeast 2 (NotHero AnyHero) (AllyControlledBy (PlayerWithId a.owner) <> ReadyAlly)

{- | The effect goes on in steps after the shuffle, as 'DoStep' carrying the total cost of
the allies exhausted: first the discard, then, with the cards it discarded as the
windows, the allies put into play.
-}
instance RunMessage AVeryGoodTale where
  runMessage msg e@(AVeryGoodTale attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      costs <- for allies \aid -> (aid,) . printedCost <$> getAlly aid
      chooseOneM pid do
        questionLabeled "aVeryGoodTale.exhaust"
        causedBy attrs
        for_ costs \(first, firstCost) -> targeting first do
          exhaust first
          chooseOneM pid do
            questionLabeled "aVeryGoodTale.exhaust"
            causedBy attrs
            for_ [c | c@(aid, _) <- costs, aid /= first] \(second, secondCost) -> targeting second do
              exhaust second
              shufflePlayerDeck pid
              push $ DoStep (firstCost + secondCost) (PlayEvent pid card [])
      pure e
    DoStep budget (PlayEvent pid card []) | card.id == toId attrs -> do
      top <- take 5 . (.deck) <$> getPlayer pid
      discardTopOfDeck pid 5
      pushWhen (notNull top)
        $ DoStep budget (PlayEvent pid card [mkAfter $ DiscardedFromDeck pid c.id | c <- top])
      pure e
    DoStep budget (PlayEvent pid card windows) | card.id == toId attrs -> do
      discarded <- (.discard) <$> getPlayer pid
      let ids = [cid | Window _ (DiscardedFromDeck _ cid) <- windows]
          allies = [pc | PlayerCard pc <- discarded, pc.id `elem` ids, cdCardType (toCardDef pc) == AllyType]
      candidates <- filterM (passesUnique . PlayerCard) allies
      let putIntoPlay :: ReverseQueue n => Int -> Int -> [PlayerCard] -> n ()
          putIntoPlay left n options =
            let affordable = [pc | pc <- options, printedCost pc <= left]
             in when (n > 0 && notNull affordable) $ chooseOneM pid do
                  questionLabeledWith "aVeryGoodTale.putIntoPlay" [intVar "count" left]
                  causedBy attrs
                  for_ affordable \pc -> targeting pc do
                    removeFromDiscard pid pc.id
                    putAllyIntoPlayUnder pid pc
                    -- a second copy of a unique ally could not enter play beside the first
                    let others =
                          [ o
                          | o <- options
                          , o.id /= pc.id
                          , not (cdUnique (toCardDef o) && cdName (toCardDef o) == cdName (toCardDef pc))
                          ]
                    putIntoPlay (left - printedCost pc) (n - 1) others
                  labeled "aVeryGoodTale.done" (pure ())
      putIntoPlay budget 2 candidates
      pure e
    _ -> AVeryGoodTale <$> runMessage msg attrs
   where
    printedCost :: HasCardDef a => a -> Int
    printedCost a = case cdCost (toCardDef a) of
      Just (StaticCost x) -> x
      _ -> 0
