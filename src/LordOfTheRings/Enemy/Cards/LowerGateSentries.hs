module LordOfTheRings.Enemy.Cards.LowerGateSentries (lowerGateSentries) where

import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype LowerGateSentries = LowerGateSentries EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lowerGateSentries :: EnemyCard LowerGateSentries
lowerGateSentries = enemy LowerGateSentries Cards.lowerGateSentries

instance HasModifiersFor LowerGateSentries where
  getModifiersFor (LowerGateSentries a) = do
    rings <- ringStates
    for_ rings \(ring, _) -> modified a ring [DoesNotReadyDuringRefresh]

instance RunMessage LowerGateSentries where
  runMessage msg e@(LowerGateSentries attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      exhausted <- any snd <$> ringStates
      enemyGetsAttack attrs eid (if exhausted then 3 else 1)
      pure e
    _ -> LowerGateSentries <$> runMessage msg attrs

-- Both printings are named by the card's text, including the treasure printing.
ringStates :: HasGame m => m [(Target, Bool)]
ringStates = getsGame \g ->
  [ (toTarget o, o.exhausted)
  | o <- toList g.entities.objectives
  , cardMatches (CardWithTitle "Bilbo's Magic Ring") o
  ]
    <> [ (toTarget a, attachmentExhausted $ toAttrs a)
       | a <- toList g.entities.attachments
       , cardMatches (CardWithTitle "Bilbo's Magic Ring") a
       ]
