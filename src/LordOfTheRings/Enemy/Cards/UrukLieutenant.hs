module LordOfTheRings.Enemy.Cards.UrukLieutenant (urukLieutenant) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype UrukLieutenant = UrukLieutenant EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukLieutenant :: EnemyCard UrukLieutenant
urukLieutenant = enemy UrukLieutenant Cards.urukLieutenant

-- | When Revealed: Add the topmost Orc enemy from the encounter discard pile to the
-- staging area.
-- Shadow: Defending player discards an attachment he controls. (All attachments he
-- controls instead if undefended.)
instance RunMessage UrukLieutenant where
  runMessage msg e@(UrukLieutenant attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (find (cardMatches $ CardWithType EnemyType <> CardWithTrait Orc) pile) addToStagingFromDiscard
      pure e
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      ifUndefended defenders (traverse_ discard attachments) (chooseAndDiscard attrs pid attachments)
      pure e
    _ -> UrukLieutenant <$> runMessage msg attrs
