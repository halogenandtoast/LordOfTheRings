module LordOfTheRings.Enemy.Cards.MordorCaveTroll (mordorCaveTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MordorCaveTroll = MordorCaveTroll EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorCaveTroll :: EnemyCard MordorCaveTroll
mordorCaveTroll = enemy MordorCaveTroll Cards.mordorCaveTroll

-- | Cannot have attachments.
instance HasModifiersFor MordorCaveTroll where
  getModifiersFor (MordorCaveTroll a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Mordor Cave Troll attacks and destroys a character, discard the top card
-- of your deck for each point of excess damage dealt by this attack. Read as the excess is
-- dealt, the character it went past already destroyed, as Stone-troll's; "your" deck is
-- that of the destroyed character's controller, the player attacked.
instance HasAbilities MordorCaveTroll where
  getAbilities (MordorCaveTroll a) = [mkAbility a 1 $ forced $ EnemyDealtExcessDamageWindow After a.id]

instance RunMessage MordorCaveTroll where
  runMessage msg e@(MordorCaveTroll attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      destroyed <- attackDestroyed attrs.id
      for_ [(t, n) | Window After (AttackDealtExcessDamage eid t n) <- ws, eid == attrs.id] \(t, n) ->
        for_ [p | d <- destroyed, d.target == t, Just p <- [d.controller]] \p ->
          discardTopOfDeck p n
      pure e
    _ -> MordorCaveTroll <$> runMessage msg attrs
