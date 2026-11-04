module LordOfTheRings.Treachery.Cards.RaiseTheAlarm (raiseTheAlarm) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.Character (getDefenseAgainst)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Trait (Trait (Raider))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RaiseTheAlarm = RaiseTheAlarm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

raiseTheAlarm :: TreacheryCard RaiseTheAlarm
raiseTheAlarm = treachery RaiseTheAlarm Cards.raiseTheAlarm

raiderCard :: CardMatcher
raiderCard = CardWithType EnemyType <> CardWithTrait Raider

-- | When Revealed: Discard cards from the top of the encounter deck until a Raider enemy is
-- discarded. Add that enemy to the staging area and place 3 resources on it.
-- Shadow: If attacking enemy is a Raider, after this attack ends, add 1 resource to
-- attacking enemy for each damage dealt by this attack.
instance RunMessage RaiseTheAlarm where
  runMessage msg t@(RaiseTheAlarm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardEncounterCardsUntil attrs raiderCard
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches raiderCard) cards) \card -> do
        addToStagingFromDiscard card
        placeResources attrs (CardIdTarget card.id) 3
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) do
        attack <- getEnemyAttack eid
        defense <- sum <$> traverse (getDefenseAgainst eid) defenders
        let n = max 0 (attack - defense)
        when (n > 0) $ afterAttack eid [PlaceTokens (CardIdSource cid) (EnemyTarget eid) Resource n]
      pure t
    _ -> RaiseTheAlarm <$> runMessage msg attrs
