module LordOfTheRings.Treachery.Cards.Discovered (discovered) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (placeUnderOrcsSearch, scourCardsInPlay, triggerEachScour)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Discovered = Discovered TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

discovered :: TreacheryCard Discovered
discovered = treachery Discovered Cards.discovered

-- | When Revealed: Either trigger each Scour effect currently in play, or each player places a
-- random card from his hand facedown underneath The Orcs' Search. The first player chooses
-- one that does something: a Scour in play, or a card in some player's hand.
-- Shadow: If this attack destroys a character, place that character facedown underneath The
-- Orcs' Search. Once the attack is over the shadow takes, from its owner's discard pile, each
-- character it could have destroyed that has left play.
-- ponytail: a character the attack could destroy that left play during it some other way
-- counts as destroyed, as Morgul Vale's shadow reads it.
instance RunMessage Discovered where
  runMessage msg t@(Discovered attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      scouring <- notNull <$> scourCardsInPlay
      hands <- any (notNull . (.hand)) <$> (traverse getPlayer =<< getRemainingPlayers)
      chooseOrRunOneM lead do
        questionLabeled "discovered.choose"
        causedBy attrs
        when scouring $ labeled "discovered.scour" triggerEachScour
        when hands $ labeled "discovered.hand" $ eachPlayer \pid -> do
          hand <- (.hand) <$> getPlayer pid
          traverse_ placeUnderOrcsSearch . headMay =<< shuffleM hand
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      cards <- traverse characterCard exposed
      afterAttack eid [IfAnyLeftPlay [c] [DoStep 1 (ResolveShadowEffect cid eid pid [CardIdTarget card])] | (c, Just card) <- zip exposed cards]
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ [CardIdTarget card]) | cid == attrs.cardId -> do
      discarded <- getsGame (concatMap (.discard) . toList . (.players))
      traverse_ placeUnderOrcsSearch $ find ((== card) . (.id)) discarded
      pure t
    _ -> Discovered <$> runMessage msg attrs
   where
    characterCard c = case c of
      HeroTarget hid -> Just . (.id) . toCard <$> getHero hid
      AllyTarget aid -> Just . (.id) . toCard <$> getAlly aid
      _ -> pure Nothing
