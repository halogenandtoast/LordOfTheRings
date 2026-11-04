module LordOfTheRings.Contract.Cards.MessengerOfTheKing (messengerOfTheKing) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ally.Types (AllyAttrs (allyCardId))
import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType), Sphere (Neutral), cdAttack, cdDefense, cdHitPoints, cdWillpower, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Entities (entitiesAllies)
import LordOfTheRings.Game.Base (gameEntities, gamePhase, gameRound)
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype MessengerOfTheKing = MessengerOfTheKing ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Side A: You cannot choose more than 2 heroes during setup (checked as the deck is
-- chosen, 'LordOfTheRings.Decklist').
messengerOfTheKing :: ContractCard MessengerOfTheKing
messengerOfTheKing = contract MessengerOfTheKing Cards.messengerOfTheKing

-- | The round and phase its character last readied in, in the meta.
lastReadied :: ContractAttrs -> Maybe (Int, Phase)
lastReadied a = parseMaybe parseJSON a.meta

{- | Side B: Attached character cannot be readied more than once per phase. Messenger of
the King is immune to card effects while attached character is in play. (The ally lost the
ally card type and gained the hero card type as this was attached: see 'AllyBecomesHero'.)
-}
instance HasModifiersFor MessengerOfTheKing where
  getModifiersFor (MessengerOfTheKing a) = case a.placement of
    AttachedTo host | a.flipped -> do
      modifySelf a [ImmuneToCardEffects]
      now <- getsGame \g -> (gameRound g, gamePhase g)
      when (lastReadied a == Just now) $ modified a host [CannotReady]
    _ -> pure ()

instance HasAbilities MessengerOfTheKing

instance RunMessage MessengerOfTheKing where
  runMessage msg c@(MessengerOfTheKing attrs) = case msg of
    -- Side A: Setup: Choose a non-neutral unique ally from your deck and put it into play.
    -- Add the sum of that ally's printed Willpower, Attack, Defense, and hit points to your
    -- starting threat. Then, flip this card over and attach it to that ally. (A searched
    -- deck is shuffled after, FAQ 1.21.)
    DoStep 3 Setup | not attrs.flipped -> do
      let pid = attrs.owner
          eligible pc =
            cardMatches (CardWithType AllyType <> UniqueCard <> NotCard (CardWithSphere Neutral)) pc
      allies <- filter eligible . (\p -> [pc | PlayerCard pc <- p.deck]) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "messengerOfTheKing.chooseAlly"
        causedBy attrs
        showing (map PlayerCard allies)
        for_ allies \pc -> targeting (CardIdTarget pc.id) do
          removeFromDeck pid pc.id
          putAllyIntoPlayUnder pid pc
          doStep 1 (WithTarget (CardIdTarget pc.id) msg)
      shufflePlayerDeck pid
      pure c
    DoStep 1 (WithTarget (CardIdTarget cid) (DoStep 3 Setup)) -> do
      found <- getsGame (find ((== cid) . allyCardId . toAttrs) . toList . entitiesAllies . gameEntities)
      case found of
        Just ally -> do
          let def = toCardDef ally
              printed = sum $ mapMaybe ($ def) [cdWillpower, cdAttack, cdDefense, cdHitPoints]
          addToStartingThreat attrs.owner printed
          flipOver attrs
          allyBecomesHero ally
          pure $ MessengerOfTheKing $ attrs {contractPlacement = AttachedTo (toTarget ally)}
        Nothing -> pure c
    -- If attached character leaves play, remove Messenger of the King from the game.
    CheckWindows ws
      | AttachedTo host <- attrs.placement
      , attrs.flipped
      , notNull [() | Window After (LeftPlay t _ _) <- ws, t == host] -> do
          removeFromGame attrs
          pure c
      | AttachedTo host <- attrs.placement
      , attrs.flipped
      , notNull [() | Window After (BecameReady t) <- ws, t == host] -> do
          now <- getsGame \g -> (gameRound g, gamePhase g)
          pure $ MessengerOfTheKing $ attrs {contractMeta = toJSON now}
    _ -> MessengerOfTheKing <$> runMessage msg attrs
