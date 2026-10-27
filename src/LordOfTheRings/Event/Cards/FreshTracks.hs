module LordOfTheRings.Event.Cards.FreshTracks (freshTracks) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype FreshTracks = FreshTracks EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an enemy is added to the staging area, deal 1 damage to that enemy.
-- Players ignore that enemy while making engagement checks this round.
freshTracks :: EventCard FreshTracks
freshTracks = eventWith FreshTracks Cards.freshTracks (windowL ?~ EnemyEntersStagingAreaWindow After)

instance RunMessage FreshTracks where
  runMessage msg e@(FreshTracks attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- windows]) \eid -> do
        dealDamage attrs eid 1
        roundModifier attrs eid NoEngagementChecks
      pure e
    _ -> FreshTracks <$> runMessage msg attrs
