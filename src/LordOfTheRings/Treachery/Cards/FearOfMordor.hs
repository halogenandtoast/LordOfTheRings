module LordOfTheRings.Treachery.Cards.FearOfMordor (fearOfMordor) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FearOfMordor = FearOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fearOfMordor :: TreacheryCard FearOfMordor
fearOfMordor = treachery FearOfMordor Cards.fearOfMordor

-- | When Revealed: Each ally in play gets -1 willpower, -1 attack, and -1 defense until the
-- end of the round.
-- Shadow: If attacking enemy's threat is greater than the defending character's willpower,
-- it makes an additional attack after this one. Undefended, there is no defending
-- character, and it does not.
instance RunMessage FearOfMordor where
  runMessage msg t@(FearOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- select AnyAlly
      for_ allies \al -> roundModifiers attrs al [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      threat <- getEnemyThreat eid
      willpowers <- traverse getWillpower defenders
      when (any (< threat) willpowers) $ additionalAttack pid eid
      pure t
    _ -> FearOfMordor <$> runMessage msg attrs
