module LordOfTheRings.Ally.Cards.AndrathGuardsman (andrathGuardsman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype AndrathGuardsman = AndrathGuardsman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

andrathGuardsman :: AllyCard AndrathGuardsman
andrathGuardsman = ally AndrathGuardsman Cards.andrathGuardsman

engagedWith :: PlayerId -> EnemyMatcher
engagedWith pid = EnemyEngagedWith (PlayerWithId pid) <> not_ UniqueEnemy

-- | Response: After you play Andrath Guardsman from your hand, choose a non-unique enemy
-- engaged with you. That enemy cannot attack you this round.
instance HasAbilities AndrathGuardsman where
  getAbilities (AndrathGuardsman a) =
    [ restricted a 1 (exists $ engagedWith a.controller)
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage AndrathGuardsman where
  runMessage msg s@(AndrathGuardsman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (engagedWith pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> roundModifier a e (CannotAttackPlayer pid)
      pure s
    _ -> AndrathGuardsman <$> runMessage msg a
