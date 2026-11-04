module LordOfTheRings.Objective.Cards.FrodosChoice (frodosChoice) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype FrodosChoice = FrodosChoice ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodosChoice :: ObjectiveCard FrodosChoice
frodosChoice = objective FrodosChoice Cards.frodosChoice

{- | When Revealed: Take control of the first player token and the Ring-bearer (and each card
attached to it). Attach to the Ring-bearer. For the rest of the game, you cannot lose control
of the first player token.

The Ring-bearer, removed from the game by stage 2A, waits as this card's captive (see
'LordOfTheRings.Quest.Cards.TheCompanyDivided'): he comes back under your control. "If
Frodo's Choice is discarded, reveal it instead" is the scenario's to resolve.
-}
instance RunMessage FrodosChoice where
  runMessage msg o@(FrodosChoice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      bearers <- charactersGuardedBy attrs.cardId
      for_ [h | HeroTarget h <- bearers] \h -> do
        -- he is no captive any more, though not rescued: no window opens for it
        push $ RescueCharacter (HeroTarget h)
        owner <- (.owner) <$> getHero h
        when (owner /= you) $ push $ ChangeHeroController h you
      lead <- getLead
      when (lead /= you) $ push $ SetFirstPlayer you
      for_ [h | HeroTarget h <- bearers] (attachObjective attrs.id)
      createEffect attrs (PlayerTarget you) [CannotLoseFirstPlayerToken] UntilSpent
      pure o
    _ -> FrodosChoice <$> runMessage msg attrs
