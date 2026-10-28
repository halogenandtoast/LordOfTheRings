module LordOfTheRings.Treachery.Cards.ScourgeOfMordor (scourgeOfMordor) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ScourgeOfMordor = ScourgeOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

scourgeOfMordor :: TreacheryCard ScourgeOfMordor
scourgeOfMordor = treachery ScourgeOfMordor Cards.scourgeOfMordor

-- | When Revealed: Each player discards the top card of his deck. Until the end of the
-- phase, raise the total threat in the staging area by X, where X is the total cost of all
-- cards discarded by this effect.
-- Shadow: attacking enemy gets +1 attack. Deal it another shadow card.
instance RunMessage ScourgeOfMordor where
  runMessage msg t@(ScourgeOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      let x = sum [n | p <- players, c <- take 1 p.deck, Just (StaticCost n) <- [cdCost (toCardDef c)]]
      for_ players \p -> discardTopOfDeck p.id 1
      phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      dealShadowCardsDuringAttack eid pid defenders 1
      pure t
    _ -> ScourgeOfMordor <$> runMessage msg attrs
