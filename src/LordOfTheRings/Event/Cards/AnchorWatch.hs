module LordOfTheRings.Event.Cards.AnchorWatch (anchorWatch) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype AnchorWatch = AnchorWatch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Exhausted characters the player controls, not yet defending.
watchers :: PlayerId -> (HeroMatcher, AllyMatcher)
watchers pid =
  ( HeroControlledBy (PlayerWithId pid) <> ExhaustedHero <> not_ DefendingHero
  , AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly <> not_ DefendingAlly
  )

-- | Response: After an enemy is declared as an attacker against you, declare an exhausted
-- character you control as the defender. Resolve that effect again for each copy of
-- Anchor Watch currently in your discard pile (all chosen characters are defending against
-- this attack).
anchorWatch :: EventCard AnchorWatch
anchorWatch =
  eventWith AnchorWatch Cards.anchorWatch \a ->
    a
      & windowL
      ?~ EnemyAttacksPlayerWindow When a.owner AnyEnemy
      & criteriaL
      .~ uncurry (CharactersAtLeast 1) (watchers a.owner)

-- | Each time the effect resolves is a later step, carrying how many are still to come.
instance RunMessage AnchorWatch where
  runMessage msg e@(AnchorWatch attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      again <- copiesInDiscard pid card
      doStep again msg
      pure e
    DoStep n msg'@(PlayEvent pid card windows) | card.id == toId attrs -> do
      for_ (take 1 [eid | Window _ (EnemyAttacking eid _) <- windows]) \eid -> do
        characters <- uncurry selectCharacters (watchers pid)
        chooseOneM pid do
          questionLabeled "anchorWatch.defender"
          causedBy attrs
          targets characters (declareAsDefender pid eid)
      when (n > 0) $ doStep (n - 1) msg'
      pure e
    _ -> AnchorWatch <$> runMessage msg attrs
