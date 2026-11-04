module LordOfTheRings.Treachery.Cards.WeCannotGetOut (weCannotGetOut) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeCannotGetOut = WeCannotGetOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weCannotGetOut :: TreacheryCard WeCannotGetOut
weCannotGetOut = treachery WeCannotGetOut Cards.weCannotGetOut

{- | When Revealed: Each player assigns X damage among characters he controls. X is the
number of exhausted characters he controls.
Shadow: Assign X damage among characters you control. X is the number of enemies engaged
with you.
-}
instance RunMessage WeCannotGetOut where
  runMessage msg t@(WeCannotGetOut attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        exhausted <- length <$> selectCharacters (ExhaustedHero <> HeroControlledBy (PlayerWithId p)) (ExhaustedAlly <> AllyControlledBy (PlayerWithId p))
        when (exhausted > 0) $ dealDamageDivided attrs p exhausted
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      when (engaged > 0) $ dealDamageDivided attrs pid engaged
      pure t
    _ -> WeCannotGetOut <$> runMessage msg attrs
