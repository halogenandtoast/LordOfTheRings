module LordOfTheRings.Event.Cards.GaldorsCounsel (
  galdorsCounselLeadership,
  galdorsCounselTactics,
  galdorsCounselSpirit,
  galdorsCounselLore,
) where

import LordOfTheRings.Card (Card, toCardCode)
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Campaign (campaignLevel)

-- | Galdor's Counsel, a scalable boon of the Dream-chaser campaign, one for each sphere.
newtype GaldorsCounsel = GaldorsCounsel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

galdorsCounselLeadership :: EventCard GaldorsCounsel
galdorsCounselLeadership = event GaldorsCounsel Cards.galdorsCounselLeadership

galdorsCounselTactics :: EventCard GaldorsCounsel
galdorsCounselTactics = event GaldorsCounsel Cards.galdorsCounselTactics

galdorsCounselSpirit :: EventCard GaldorsCounsel
galdorsCounselSpirit = event GaldorsCounsel Cards.galdorsCounselSpirit

galdorsCounselLore :: EventCard GaldorsCounsel
galdorsCounselLore = event GaldorsCounsel Cards.galdorsCounselLore

halfUp :: Int -> Int
halfUp n = (n + 1) `div` 2

{- | Each adds Galdor's Counsel to the victory display.
Leadership: Action: Add X resources to any hero's resource pool. X is half the level,
rounded up.
Tactics: Action: Deal X damage to a non-unique enemy. X is the level.
Spirit: Action: Place X progress on a non-unique location in the staging area. X is the
level.
Lore: Action: Look at the top X or bottom X cards of your deck, add any number of them to
your hand, and put the rest on the top and/or bottom of your deck in any order. X is half
the level, rounded up.
-}
instance RunMessage GaldorsCounsel where
  runMessage msg e@(GaldorsCounsel attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      lvl <- campaignLevel card
      case unCardCode (toCardCode card) of
        "DCC184" -> do
          heroes <- select AnyHero
          chooseOrRunOneM pid do
            questionLabeled "chooseHero"
            causedBy attrs
            targets heroes \h -> placeResources attrs h (halfUp lvl)
        "DCC185" -> do
          enemies <- select (not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards)
          chooseOrRunOneM pid do
            questionLabeled "galdorsCounsel.enemy"
            causedBy attrs
            targets enemies \en -> dealDamage attrs en lvl
        "DCC186" -> do
          locations <- select (LocationInStagingArea <> LocationNotImmuneToPlayerCards <> not_ UniqueLocation)
          chooseOrRunOneM pid do
            questionLabeled "galdorsCounsel.location"
            causedBy attrs
            targets locations \l -> placeProgress attrs l lvl
        _ -> do
          player <- getPlayer pid
          let x = halfUp lvl
          chooseOneM pid do
            questionLabeled "galdorsCounsel.topOrBottom"
            causedBy attrs
            labeled "galdorsCounsel.top" $ lookAt pid attrs (take x player.deck)
            labeled "galdorsCounsel.bottom" $ lookAt pid attrs (reverse $ take x $ reverse player.deck)
      addToVictoryDisplay card
      pure e
    _ -> GaldorsCounsel <$> runMessage msg attrs

-- | The player adds any number of the cards looked at to their hand, one at a time, then
-- puts the rest back.
lookAt :: ReverseQueue m => PlayerId -> EventAttrs -> [Card] -> m ()
lookAt pid attrs cards = when (notNull cards) $ chooseOneM pid do
  questionLabeled "galdorsCounsel.addToHand"
  causedBy attrs
  showing cards
  labeled "galdorsCounsel.done" $ putBack pid attrs cards
  targets cards \c -> do
    takeFromDeck pid c
    lookAt pid attrs (filter (/= c) cards)

-- | Each card in turn goes on the top or the bottom of the deck, as the player chooses: the
-- last put on top ends up on top, the last put on the bottom at the bottom, so any order
-- can be had.
putBack :: ReverseQueue m => PlayerId -> EventAttrs -> [Card] -> m ()
putBack pid attrs cards = chooseOneAtATimeM pid do
  questionLabeled "galdorsCounsel.putBack"
  causedBy attrs
  showing cards
  targets cards \c -> chooseOneM pid do
    questionLabeled "galdorsCounsel.topOrBottomOfDeck"
    causedBy attrs
    showing [c]
    labeled "galdorsCounsel.onTop" $ moveToTopOfDeck pid c
    labeled "galdorsCounsel.onBottom" $ moveToBottomOfDeck pid c
