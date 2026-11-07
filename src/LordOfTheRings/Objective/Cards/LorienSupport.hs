module LordOfTheRings.Objective.Cards.LorienSupport (powerOfLorien, hostOfLorien) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (giveLorienSupport)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype LorienSupport = LorienSupport ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

powerOfLorien, hostOfLorien :: ObjectiveCard LorienSupport
powerOfLorien = objective LorienSupport Cards.powerOfLorien
hostOfLorien = objective LorienSupport Cards.hostOfLorien

instance HasModifiersFor LorienSupport where
  getModifiersFor (LorienSupport a)
    -- Caleb Grace, 27 May 2022 (ALeP FAQ 3.373): discarded Power enters
    -- the encounter discard pile; it can later be revealed as a blank objective.
    | toCardCode a == toCardCode Cards.powerOfLorien = modifySelf a [LeavesPlayToEncounterDiscard]
    | otherwise = modifySelf a [ImmuneToPlayerCardEffects, RemoveFromGameIfLeavesPlay]

instance HasAbilities LorienSupport where
  getAbilities (LorienSupport a) =
    if toCardCode a == toCardCode Cards.powerOfLorien
      && (case a.placement of AttachedTo (PlayerTarget _) -> True; _ -> False)
      then
        [ restricted a 1 EncounterEffectsCancelable
            $ ResponseAbility (EncounterCardRevealedWindow When) $ AddToVictoryDisplayCost $ toTarget a
        , restricted a 2 InEpicMultiplayer $ response RoundEndsWindow
        ]
      else []

instance RunMessage LorienSupport where
  runMessage msg o@(LorienSupport a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [target | Window When (Revealed target) <- windows]) $ \target ->
        cancelEncounterCard pid target $ discard a target
      pure o
    UseCardAbility pid (isSource a -> True) 2 _ -> o <$ giveLorienSupport msg pid a
    _ -> LorienSupport <$> runMessage msg a
