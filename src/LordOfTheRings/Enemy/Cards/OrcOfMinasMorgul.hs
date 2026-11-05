module LordOfTheRings.Enemy.Cards.OrcOfMinasMorgul (orcOfMinasMorgul) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)

newtype OrcOfMinasMorgul = OrcOfMinasMorgul EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfMinasMorgul :: EnemyCard OrcOfMinasMorgul
orcOfMinasMorgul = enemy OrcOfMinasMorgul Cards.orcOfMinasMorgul

instance RunMessage OrcOfMinasMorgul where
  runMessage msg e@(OrcOfMinasMorgul a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pid <- getRevealer
      hand <- discardableHand <$> getPlayer pid
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      chooseOrRunOneM pid do
        questionLabeled "orcOfMinasMorgul.choose"
        causedBy a
        when (notNull hand && not barred) $ labeled "orcOfMinasMorgul.discard" $ discardRandomFromHand pid 1
        labeled "orcOfMinasMorgul.engage" do
          engageEnemy pid a.id
          revealEncounterCardAs pid
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 2
      pure e
    _ -> OrcOfMinasMorgul <$> runMessage msg a
