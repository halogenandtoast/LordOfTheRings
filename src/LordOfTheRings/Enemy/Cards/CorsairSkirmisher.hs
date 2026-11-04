module LordOfTheRings.Enemy.Cards.CorsairSkirmisher (corsairSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype CorsairSkirmisher = CorsairSkirmisher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairSkirmisher :: EnemyCard CorsairSkirmisher
corsairSkirmisher = enemy CorsairSkirmisher Cards.corsairSkirmisher

-- | Boarding 1. Immune to player card effects. Destroyed, it stays in play until its
-- Response is answered, then goes to the discard pile unless taken.
instance HasModifiersFor CorsairSkirmisher where
  getModifiersFor (CorsairSkirmisher a) = modified a a [ImmuneToPlayerCardEffects, StaysInPlayWhenDefeated]

{- | Response: After Corsair Skirmisher is destroyed as an enemy, the engaged player may
discard allies he controls with a total of at least 3 willpower to take control of
Corsair Skirmisher as a Ship objective with willpower equal to its threat.
Answered in the window after it is destroyed, its After windows open: taken, it turns over
to its Ship objective side, as a double-sided enemy flips, rather than coming back from the
discard pile.
-}
instance HasAbilities CorsairSkirmisher where
  getAbilities (CorsairSkirmisher a) = [mkAbility a 1 $ silent $ ThisEnemyDefeatedWindow After a.id]

-- | The allies the player may discard: not his Ship objectives.
discardable :: PlayerId -> AllyMatcher
discardable pid = AllyControlledBy (PlayerWithId pid) <> not_ ObjectiveAlly

instance RunMessage CorsairSkirmisher where
  runMessage msg e@(CorsairSkirmisher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      total <- case attrs.placement of
        EngagedWith pid -> sum <$> (traverse (getWillpower . toTarget) =<< select (discardable pid))
        _ -> pure 0
      case attrs.placement of
        EngagedWith pid | total >= 3 -> chooseOneM pid do
          questionLabeled "corsairSkirmisher.take"
          causedBy attrs
          labeled "corsairSkirmisher.discard" $ doStep 3 msg
          labeled "corsairSkirmisher.decline" $ discard attrs
        _ -> discard attrs
      pure e
    -- the allies are discarded one at a time until their willpower reaches 3
    DoStep x inner@(UseCardAbility _ (isSource attrs -> True) 1 _) | EngagedWith pid <- attrs.placement -> do
      if x <= 0
        then do
          flipOver attrs
          takeControl (AllyId $ unEnemyId attrs.id) pid
        else do
          allies <- select (discardable pid)
          chooseOneM pid do
            questionLabeledWith "corsairSkirmisher.chooseAlly" [intVar "count" x]
            causedBy attrs
            targets allies \aid -> do
              willpower <- getWillpower (toTarget aid)
              discard aid
              doStep (x - willpower) inner
      pure e
    _ -> CorsairSkirmisher <$> runMessage msg attrs
