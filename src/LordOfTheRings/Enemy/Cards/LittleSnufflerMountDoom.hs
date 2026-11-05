module LordOfTheRings.Enemy.Cards.LittleSnufflerMountDoom (littleSnufflerMountDoom) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype LittleSnufflerMountDoom = LittleSnufflerMountDoom EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

littleSnufflerMountDoom :: EnemyCard LittleSnufflerMountDoom
littleSnufflerMountDoom = enemy LittleSnufflerMountDoom Cards.littleSnufflerMountDoom

-- | Peril. Surge.
-- When Revealed: Either raise your threat by 1 for each ally you control, or discard an
-- ally you control.
-- Shadow: Exhaust a character you control.
instance RunMessage LittleSnufflerMountDoom where
  runMessage msg e@(LittleSnufflerMountDoom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      pid <- getRevealer
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "littleSnufflerMountDoom.choose"
        causedBy attrs
        labeledWith "littleSnufflerMountDoom.raiseThreat" [intVar "count" (length allies)]
          $ when (notNull allies)
          $ encounterRaisesThreat pid (length allies)
        when (notNull allies) $ labeled "littleSnufflerMountDoom.discardAlly" do
          chooseOneM pid do
            questionLabeled "littleSnufflerMountDoom.discardAlly"
            causedBy attrs
            targets allies (discard attrs)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> LittleSnufflerMountDoom <$> runMessage msg attrs
