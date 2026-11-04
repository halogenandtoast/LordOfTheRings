module LordOfTheRings.Treachery.Cards.EatenAlive (eatenAlive) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EatenAlive = EatenAlive TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Flight to the Ford burden set. Surge.
eatenAlive :: TreacheryCard EatenAlive
eatenAlive = treachery EatenAlive Cards.eatenAlive

-- | When Revealed: The first player must deal X damage divided among heroes he controls
-- where X is the number of players in the game.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage EatenAlive where
  runMessage msg t@(EatenAlive attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      players <- length <$> getRemainingPlayers
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      dealDamageDividedAmong attrs lead heroes players
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> EatenAlive <$> runMessage msg attrs
