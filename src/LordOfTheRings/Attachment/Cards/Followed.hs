module LordOfTheRings.Attachment.Cards.Followed (followed) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)

newtype Followed = Followed AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play into your play area unattached. While unattached, attach to the next eligible
-- enemy that engages you.
followed :: AttachmentCard Followed
followed = attachment Followed Cards.followed & attachTo PlayIntoPlayArea

-- | While attached enemy is engaged with you, reduce the total threat in the staging area
-- by X, where X is attached enemy's printed threat.
instance HasModifiersFor Followed where
  getModifiersFor (Followed a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> do
      engaged <- selectAny (EnemyWithId eid <> EnemyEngagedWith (PlayerWithId a.controller))
      x <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
      when (engaged && x > 0) $ modified a GameTarget [StagingThreatBonus (negate x)]
    _ -> pure ()
