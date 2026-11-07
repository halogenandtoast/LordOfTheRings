module LordOfTheRings.Objective.Cards.LorienSupport (powerOfLorien, hostOfLorien) where

import LordOfTheRings.Card (toCardCode)
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
    [ restricted a 1 EncounterEffectsCancelable
        $ ResponseAbility (EncounterCardRevealedWindow When) $ AddToVictoryDisplayCost $ toTarget a
    | toCardCode a == toCardCode Cards.powerOfLorien, AttachedTo (PlayerTarget _) <- [a.placement]
    ]

instance RunMessage LorienSupport where
  runMessage msg o@(LorienSupport a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [target | Window When (Revealed target) <- windows]) $ \target ->
        cancelEncounterCard pid target $ discard a target
      pure o
    _ -> LorienSupport <$> runMessage msg a
