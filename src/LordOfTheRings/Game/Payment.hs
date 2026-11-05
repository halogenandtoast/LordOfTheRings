module LordOfTheRings.Game.Payment (module LordOfTheRings.Game.Payment) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map (findWithDefault)
import LordOfTheRings.ActiveCost (
  ActiveCost (
    ActiveCost,
    activeCostCard,
    activeCostCost,
    activeCostDoomed,
    activeCostFromHand,
    activeCostHost,
    activeCostId,
    activeCostPaidFrom,
    activeCostPlayer,
    activeCostWindows
  ),
  Cost (ResourceCost),
  cardCost,
  raiseCost,
  staticCost,
 )
import LordOfTheRings.Card (
  Card,
  CardType (EventType),
  HasCardDef (toCardDef),
  Sphere (Neutral),
 )
import LordOfTheRings.Classes.Entity (insertEntity)
import LordOfTheRings.Classes.HasQueue (HasQueue (pushAll), push)
import LordOfTheRings.Effect (
  Effect (Effect, effectModifiers, effectSource, effectTarget),
  EffectDuration (UntilEndOfRound),
 )
import LordOfTheRings.Game.Base (
  Game (gameEffects, gameModifiers),
  activeCostsL,
  effectsL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.I18n (ikey, ikeyWith, intVar)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Matcher (
  allyPayersFor,
  cardMatches,
  getPlayer,
  keywordValue,
 )
import LordOfTheRings.Message (
  Message (
    CreateEffect,
    DiscardTopOfDeck,
    ExhaustToReduceCost,
    PayCost,
    ReduceActiveCost,
    SpendResourcesFrom
  ),
  questionLabel,
  questionLabelWith,
 )
import LordOfTheRings.Modifier (
  Modifier (Modifier),
  ModifierType (
    CostModifier,
    GainKeyword,
    LowerNextCostOf,
    LowerNextDoomed,
    MayGainDoomed,
    NextCardGainsDoomed,
    NextIgnoresResourceMatch,
    ReduceNextCostOf,
    ReducedByDiscardingFromDeck,
    ReducedByExhausting,
    SpentOnCardGives
  ),
 )
import LordOfTheRings.Player.Runner (costModifier)
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Question (UI (Label), targetLabel)
import LordOfTheRings.Source (Source (CardIdSource))
import LordOfTheRings.Target (
  Target (CardIdTarget, GameTarget, PlayerTarget),
  Targetable (toTarget),
 )
import LordOfTheRings.Window (Window)

import LordOfTheRings.Game.Modifiers (handCardModifiers)

payFor :: Game -> PlayerId -> Card -> Bool -> [Window] -> Maybe Target -> Int -> GameT Game
payFor g pid card fromHand windows host discount = do
  costId <- getRandom
  -- the card has left its owner's hand and is not yet being paid for, so the modifiers it
  -- gives itself are asked for here
  MonoidalMap own <- execWriterT $ handCardModifiers pid card
  let ownMods = Map.findWithDefault [] (CardIdTarget card.id) own
      ownExtra = sum [k | Modifier _ (CostModifier k) <- ownMods]
  extra <- (+ ownExtra) . subtract discount <$> costModifier pid card
  -- a reduction of the next matching card's cost is spent on this one, and the Doomed
  -- that comes with it, as Gríma's
  let ours e = effectTarget e `elem` [PlayerTarget pid, GameTarget]
      gainsDoomed = or [fromHand | e <- gameEffects g, ours e, NextCardGainsDoomed _ <- effectModifiers e]
  -- a lowering of the next Doomed X event's X, as Saruman's Staff's, is spent on an event
  -- with Doomed, printed or gained
  doomedNow <- keywordValue (\case Doomed k -> Just k; _ -> Nothing) (CardIdTarget card.id) (toCardDef card)
  let doomedEvent = card.cardType == EventType && (doomedNow > 0 || gainsDoomed)
      spent e =
        ours e
          && or
            ( [cardMatches m card | ReduceNextCostOf m _ <- effectModifiers e]
                <> [cardMatches m card | LowerNextCostOf m _ <- effectModifiers e]
                <> [cardMatches m card | NextIgnoresResourceMatch m <- effectModifiers e]
                <> [fromHand | NextCardGainsDoomed _ <- effectModifiers e]
                <> [doomedEvent | LowerNextDoomed _ <- effectModifiers e]
            )
      spentEffects = filter spent (gameEffects g)
      -- one that does not require a resource match, as King of Dale's ally, is paid as neutral;
      -- so is one a constant ability lets off it, as The Grey Wanderer's side B, which keeps
      -- count of its own use
      unmatched =
        or [cardMatches m card | e <- spentEffects, NextIgnoresResourceMatch m <- effectModifiers e]
          || or [cardMatches m card | Modifier _ (NextIgnoresResourceMatch m) <- Map.findWithDefault [] (PlayerTarget pid) (gameModifiers g)]
      asPaid = \case
        LordOfTheRings.ActiveCost.ResourceCost _ c | unmatched -> LordOfTheRings.ActiveCost.ResourceCost Neutral c
        c -> c
  let activeCost =
        ActiveCost
          { activeCostId = costId
          , activeCostCost = raiseCost extra (asPaid $ cardCost card)
          , activeCostCard = card
          , activeCostPlayer = pid
          , activeCostWindows = windows
          , activeCostHost = host
          , activeCostFromHand = fromHand
          , activeCostDoomed = negate $ sum [n | e <- spentEffects, LowerNextDoomed n <- effectModifiers e]
          , activeCostPaidFrom = []
          }
  -- resources on allies, as Radagast's, may pay for the cards they are for
  payers <- allyPayersFor pid card
  let n = staticCost activeCost.activeCostCost
      pay left =
        if null payers || left == 0
          then PayCost costId
          else
            questionLabel (ikey "label.payFromAlly") pid
              $ [ targetLabel
                    aid
                    [ SpendResourcesFrom pid [(toTarget aid, k)]
                    , ReduceActiveCost costId k
                    , PayCost costId
                    ]
                | (aid, r) <- payers
                , k <- [1 .. min r left]
                ]
              <> [Label (ikey "label.payFromHeroes") [PayCost costId]]
  -- a card that may cost less for cards discarded from the deck, as Erebor Guard, asks
  -- first; it is offered only with the cards to discard
  deckSize <- length . (.deck) <$> getPlayer pid
  push $ case [k | Modifier _ (ReducedByDiscardingFromDeck k) <- ownMods, k <= deckSize] of
    k : _ ->
      questionLabelWith
        (ikeyWith "label.discardToReduceCost" [intVar "count" k])
        (Just $ CardIdTarget card.id)
        []
        pid
        [ Label
            (ikey "label.discardAndReduce")
            [DiscardTopOfDeck pid k, ReduceActiveCost costId k, pay (max 0 $ n - k)]
        , Label (ikey "label.payFullCost") [pay n]
        ]
    [] -> case [t | Modifier _ (ReducedByExhausting t) <- ownMods] of
      -- one that costs less for characters exhausted, as The Houses of Healing, asks first
      t : _ -> ExhaustToReduceCost pid costId t
      [] -> pay n
  -- a card its player may give Doomed as they play it from hand, as Greyflood Wanderer,
  -- asks first; what it gains it keeps for the round, for its abilities to read, and
  -- loses as it is played again, a new card
  for_ (headMay [k | fromHand, Modifier _ (MayGainDoomed k) <- ownMods]) \k ->
    push
      $ questionLabelWith
        (ikeyWith "label.giveDoomed" [intVar "count" k])
        (Just $ CardIdTarget card.id)
        []
        pid
        [ Label
            (ikey "label.giveItDoomed")
            [CreateEffect $ Effect (CardIdSource card.id) (CardIdTarget card.id) [GainKeyword (Doomed k)] UntilEndOfRound]
        , Label (ikey "label.doNotGiveDoomed") []
        ]
  pushAll
    [ CreateEffect $ Effect (effectSource e) (CardIdTarget card.id) ms UntilEndOfRound
    | e <- spentEffects
    , SpentOnCardGives ms <- effectModifiers e
    ]
  -- the Doomed it gains, as Gríma's, is on it for the round, for what reads a card with
  -- Doomed, as Saruman does; lost as it is played again, as Greyflood Wanderer's is
  pushAll
    [ CreateEffect $ Effect (CardIdSource card.id) (CardIdTarget card.id) [GainKeyword (Doomed k)] UntilEndOfRound
    | e <- spentEffects
    , NextCardGainsDoomed k <- effectModifiers e
    ]
  let gained e = effectSource e == CardIdSource card.id && effectTarget e == CardIdTarget card.id
  pure $ g & activeCostsL %~ insertEntity activeCost & effectsL %~ filter (\e -> not (spent e || gained e))

-- | The Doomed a card is printed with.
