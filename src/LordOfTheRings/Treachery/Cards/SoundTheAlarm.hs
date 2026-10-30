module LordOfTheRings.Treachery.Cards.SoundTheAlarm (soundTheAlarm) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SoundTheAlarm = SoundTheAlarm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 3, as its card def says.
soundTheAlarm :: TreacheryCard SoundTheAlarm
soundTheAlarm = treachery SoundTheAlarm Cards.soundTheAlarm

-- | When Revealed: If there are no enemies in play, search the encounter deck and discard
-- pile for an enemy and add it to the staging area. Until the end of the round, each enemy
-- gets -10 engagement cost and +1 attack.
instance RunMessage SoundTheAlarm where
  runMessage msg t@(SoundTheAlarm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      none <- selectNone AnyEnemy
      when none do
        lead <- getLead
        searchEncounterDeckAndDiscard lead attrs (CardWithType EnemyType)
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      enemies <- select AnyEnemy
      for_ enemies \e -> roundModifiers attrs e [EngagementCostModifier (-10), AttackModifier 1]
      pure t
    _ -> SoundTheAlarm <$> runMessage msg attrs
