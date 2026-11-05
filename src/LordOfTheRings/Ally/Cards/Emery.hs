module LordOfTheRings.Ally.Cards.Emery (emery) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), cdSphere, toCardDef)
import LordOfTheRings.Sphere (Sphere (..))

newtype Emery = Emery AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Action: Discard the top 3 cards of your deck to put Emery into play from your hand,
-- under any player's control. Then, if any of the discarded cards have the tactics, lore
-- or leadership sphere, discard Emery.
emery :: AllyCard Emery
emery = ally (\a -> Emery a {allyHandAction = Just $ PlayerExists (ActivePlayer <> PlayerWithCardsInDeck 3)}) Cards.emery

instance RunMessage Emery where
  runMessage msg s@(Emery a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      player <- getPlayer pid
      -- the 3 cards are its cost, so it is offered only with 3 to discard
      let discarded = take 3 player.deck
          offSphere = any ((`elem` map Just [Tactics, Lore, Leadership]) . cdSphere . toCardDef) discarded
      discardTopOfDeck pid 3
      for_ [pc | PlayerCard pc <- player.hand, pc.id == cid] \pc ->
        choosePlayerM pid (CardIdTarget cid) \p -> putAllyIntoPlayUnder p pc
      when offSphere $ discard a (CardIdTarget cid)
      pure s
    _ -> Emery <$> runMessage msg a
