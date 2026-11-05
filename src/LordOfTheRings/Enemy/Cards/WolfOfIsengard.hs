module LordOfTheRings.Enemy.Cards.WolfOfIsengard (wolfOfIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (hasWizardry)

newtype WolfOfIsengard = WolfOfIsengard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfOfIsengard :: EnemyCard WolfOfIsengard
wolfOfIsengard = enemy WolfOfIsengard Cards.wolfOfIsengard

{- | Forced: When Wolf of Isengard is dealt a shadow card with a Wizardry effect, either
discard 1 card from your hand, or Wolf of Isengard makes an additional attack against you
after this one. "You" is the player it is engaged with; with no card in hand he takes the
attack. The attack the card is dealt for is yet to begin, so the additional one waits for
that attack to resolve.
Shadow: Deal 1 damage to the defending character.
-}
instance RunMessage WolfOfIsengard where
  runMessage msg e@(WolfOfIsengard attrs) = case msg of
    CheckWindows ws -> do
      let dealt = [(cid, pid) | Window _ (ShadowCardDealt eid cid pid) <- ws, eid == attrs.id]
      for_ [pid | (cid, pid) <- dealt, c <- attrs.shadowCards, c.id == cid, hasWizardry (toCardCode c)] \pid -> do
        hand <- (.hand) <$> getPlayer pid
        chooseOrRunOneM pid do
          questionLabeled "wolfOfIsengard.choose"
          causedBy attrs
          when (notNull hand) $ labeled "wolfOfIsengard.discard" $ chooseAndDiscardFromHand pid attrs 1
          labeled "wolfOfIsengard.attack" $ phaseModifier attrs attrs (AdditionalAttackAfterNextAttack pid)
      WolfOfIsengard <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage (CardIdSource cid) d 1
      pure e
    _ -> WolfOfIsengard <$> runMessage msg attrs
