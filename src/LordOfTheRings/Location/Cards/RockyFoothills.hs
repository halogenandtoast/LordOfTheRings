module LordOfTheRings.Location.Cards.RockyFoothills (rockyFoothills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RockyFoothills = RockyFoothills LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rockyFoothills :: LocationCard RockyFoothills
rockyFoothills = location RockyFoothills Cards.rockyFoothills

-- | When faced with the option to travel, the players must travel to Rocky Foothills, if
-- able. Rocky Foothills gets +2 quest points for each quest in play.
instance HasModifiersFor RockyFoothills where
  getModifiersFor (RockyFoothills a) = do
    quests <- questCardsInPlay
    modified a a $ MustBeTraveledTo : [QuestPointModifier (2 * quests) | quests > 0]

-- | Shadow: Attacking enemy gets +1 attack for each damage on the defending character.
instance RunMessage RockyFoothills where
  runMessage msg l@(RockyFoothills a) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      damage <- sum <$> traverse getDamage defenders
      when (damage > 0) $ enemyGetsAttack (CardIdSource cid) eid damage
      pure l
    _ -> RockyFoothills <$> runMessage msg a
