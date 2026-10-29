module LordOfTheRings.Location.Cards.CityRemains (cityRemains) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch, triggerTopmostScourInDiscard)

newtype CityRemains = CityRemains LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityRemains :: LocationCard CityRemains
cityRemains = location CityRemains Cards.cityRemains

-- | Forced: After placing any amount of progress here, trigger the topmost Scour effect in
-- the discard pile, if able.
instance HasAbilities CityRemains where
  getAbilities (CityRemains a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, raise each
-- player's threat by 1 for each card underneath The Orcs' Search. Once the attack is over
-- the shadow reads whether a character it could have destroyed has left play.
-- ponytail: a character the attack could destroy that left play during it some other way
-- counts as destroyed, as Morgul Vale's shadow reads it.
instance RunMessage CityRemains where
  runMessage msg l@(CityRemains a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      triggerTopmostScourInDiscard
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      afterAttack eid [DoStep 1 (ResolveShadowEffect cid eid pid exposed)]
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ exposed) | cid == a.cardId -> do
      remaining <- selectCharacters AnyHero AnyAlly
      when (any (`notElem` remaining) exposed) do
        n <- cardsUnderOrcsSearch
        eachPlayer \p -> encounterRaisesThreat p n
      pure l
    _ -> CityRemains <$> runMessage msg a
