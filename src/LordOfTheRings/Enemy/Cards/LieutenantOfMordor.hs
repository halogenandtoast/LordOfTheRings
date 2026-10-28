module LordOfTheRings.Enemy.Cards.LieutenantOfMordor (lieutenantOfMordor) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (TreacheryType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype LieutenantOfMordor = LieutenantOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lieutenantOfMordor :: EnemyCard LieutenantOfMordor
lieutenantOfMordor = enemy LieutenantOfMordor Cards.lieutenantOfMordor

-- | Allies cannot defend against Lieutenant of Mordor.
instance HasModifiersFor LieutenantOfMordor where
  getModifiersFor (LieutenantOfMordor a) = modified a a [CannotBeDefendedByAllies]

-- | When Revealed: Resolve the "when revealed" effect on the topmost treachery card in the
-- encounter discard pile, if able. This effect cannot be canceled.
-- ponytail: the treachery is revealed again from the discard pile, its keywords too
instance RunMessage LieutenantOfMordor where
  runMessage msg e@(LieutenantOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (find ((== TreacheryType) . cdCardType . toCardDef) discarded) revealFromEncounterDiscard
      pure e
    _ -> LieutenantOfMordor <$> runMessage msg attrs
