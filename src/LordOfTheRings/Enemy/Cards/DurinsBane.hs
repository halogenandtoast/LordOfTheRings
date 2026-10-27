module LordOfTheRings.Enemy.Cards.DurinsBane (durinsBane) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DurinsBane = DurinsBane EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

durinsBane :: EnemyCard DurinsBane
durinsBane = enemy DurinsBane Cards.durinsBane

-- | Regenerate 3. Indestructible.
-- Players cannot play attachments on Durin's Bane.
-- Durin's Bane cannot leave the staging area, is considered to be engaged with each player
-- whose threat is 1 or greater, and attacks each of those players in turn during the
-- combat phase (deal and discard a new shadow card each attack).
instance HasModifiersFor DurinsBane where
  getModifiersFor (DurinsBane a) =
    modifySelf
      a
      [ CannotHaveAttachments
      , NoEngagementChecks
      , CannotBeOptionallyEngagedBy AnyPlayer
      , EngagedWithEachPlayer (PlayerWithThreatAtLeast 1)
      ]

instance RunMessage DurinsBane where
  runMessage msg e@(DurinsBane attrs) = case msg of
    -- it cannot leave the staging area: an effect that would engage it does not move it
    PlaceEnemy eid placement | eid == attrs.id && placement /= StagingArea -> pure e
    _ -> DurinsBane <$> runMessage msg attrs
