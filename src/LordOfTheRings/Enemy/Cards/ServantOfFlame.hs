module LordOfTheRings.Enemy.Cards.ServantOfFlame (servantOfFlame) where

import LordOfTheRings.Card (CardType (TreacheryType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (getDurinsBane)
import LordOfTheRings.Trait (Trait (Flame, Shadow))

newtype ServantOfFlame = ServantOfFlame EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

servantOfFlame :: EnemyCard ServantOfFlame
servantOfFlame = enemy ServantOfFlame Cards.servantOfFlame

-- | While Durin's Bane has no damage, Servant of Flame gets +2 [Attack] and +2 [Defense].
instance HasModifiersFor ServantOfFlame where
  getModifiersFor (ServantOfFlame a) = do
    bane <- getDurinsBane
    unharmed <- maybe (pure False) (fmap (== 0) . getDamage . toTarget) bane
    when unharmed $ modifySelf a [AttackModifier 2, DefenseModifier 2]

-- | Shadow: Search the encounter deck for a treachery card with the Shadow or Flame trait
-- and reveal it, if able. Shuffle the encounter deck.
instance RunMessage ServantOfFlame where
  runMessage msg e@(ServantOfFlame attrs) = case msg of
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let wanted = CardWithType TreacheryType <> CardMatchAny [CardWithTrait Shadow, CardWithTrait Flame]
      for_ (find (cardMatches wanted) deck) \c -> do
        removeFromEncounterDeck c
        push $ RevealCard c
      shuffleEncounterDeck
      pure e
    _ -> ServantOfFlame <$> runMessage msg attrs
