module LordOfTheRings.Enemy.Cards.TelemnarsBane (telemnarsBane) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TelemnarsBane = TelemnarsBane EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

telemnarsBane :: EnemyCard TelemnarsBane
telemnarsBane = enemy TelemnarsBane Cards.telemnarsBane

-- | Forced: When Telemnar's Bane attacks, discard the top 3 cards of each player's deck.
-- Forced: After a player plays an event card, Telemnar's Bane makes an immediate attack
-- against the first player.
-- ponytail: read off events played from hand; one played from elsewhere does not trigger it.
instance HasAbilities TelemnarsBane where
  getAbilities (TelemnarsBane a) =
    [ mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id
    , mkAbility a 2 $ forced $ CardPlayedWindow After (CardWithType EventType)
    ]

instance RunMessage TelemnarsBane where
  runMessage msg e@(TelemnarsBane attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfDeck p 3
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> TelemnarsBane <$> runMessage msg attrs
