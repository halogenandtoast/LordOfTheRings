module LordOfTheRings.Location.Cards.GapOfRohan (gapOfRohan) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dunland))

newtype GapOfRohan = GapOfRohan LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gapOfRohan :: LocationCard GapOfRohan
gapOfRohan = location GapOfRohan Cards.gapOfRohan

-- | While Gap of Rohan is in the staging area, Dunland enemies get +1 attack.
instance HasModifiersFor GapOfRohan where
  getModifiersFor (GapOfRohan a) = when (a.placement == StagingArea) do
    enemies <- select (EnemyWithTrait Dunland)
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, remove 1
-- time counter from the current quest.
-- Once the attack is over the shadow reads whether any character it could have destroyed,
-- a defender or, undefended, one of the attacked player's, has left play.
-- ponytail: a character the attack could destroy that left play during it some other way,
-- as a defender discarded by a player card, counts as destroyed, as Morgul Vale's does.
instance RunMessage GapOfRohan where
  runMessage msg l@(GapOfRohan a) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      afterAttack eid [DoStep 1 (ResolveShadowEffect cid eid pid exposed)]
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ exposed) | cid == a.cardId -> do
      remaining <- selectCharacters AnyHero AnyAlly
      when (any (`notElem` remaining) exposed) $ removeTimeCountersFromCurrentQuest a 1
      pure l
    _ -> GapOfRohan <$> runMessage msg a
