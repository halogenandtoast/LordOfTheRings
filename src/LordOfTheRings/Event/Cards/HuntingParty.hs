module LordOfTheRings.Event.Cards.HuntingParty (huntingParty) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Scout, Warrior))

newtype HuntingParty = HuntingParty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Warrior trait and another unique
-- character with the Scout trait. Response: After the players have committed characters
-- to the quest (as the staging step begins), discard a non-unique enemy in the staging
-- area. Then, reveal an encounter card.
huntingParty :: EventCard HuntingParty
huntingParty =
  eventWith HuntingParty Cards.huntingParty \a ->
    a
      & windowL ?~ StagingStepBeginsWindow
      & criteriaL .~ (uniqueCharactersWith a.owner [Warrior, Scout] <> exists staged)

staged :: EnemyMatcher
staged = EnemyInStagingArea <> not_ UniqueEnemy

instance RunMessage HuntingParty where
  runMessage msg e@(HuntingParty attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select staged
      chooseOrRunOneM pid do
        questionLabeled "huntingParty.discard"
        causedBy attrs
        targets enemies discard
      revealEncounterCard
      pure e
    _ -> HuntingParty <$> runMessage msg attrs
