module LordOfTheRings.Treachery.Cards.TakingOnWater (takingOnWater) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TakingOnWater = TakingOnWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

takingOnWater :: TreacheryCard TakingOnWater
takingOnWater = treachery TakingOnWater Cards.takingOnWater

-- | Counts as a Condition attachment with the text: "Forced: When attached objective
-- exhausts, deal 2 damage to it. Action: Exhaust 3 characters you control to discard
-- Taking on Water."
instance HasAbilities TakingOnWater where
  getAbilities (TakingOnWater a) = case a.placement of
    AttachedTo host ->
      [ mkAbility a 1 $ forced $ ExhaustedWindow After host
      , anyPlayer
          $ restricted a 2 (CharactersAtLeast 3 (HeroControlledBy ActivePlayer <> ReadyHero) (AllyControlledBy ActivePlayer <> ReadyAlly))
          $ ActionAbility
          $ Costs (replicate 3 $ ExhaustCharacterCost (HeroControlledBy ActivePlayer) (AllyControlledBy ActivePlayer))
      ]
    _ -> []

-- | When Revealed: Attach to a Ship objective.
-- Shadow: If defending character is a Ship, attach Taking on Water to it.
instance RunMessage TakingOnWater where
  runMessage msg t@(TakingOnWater attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      ships <- select shipObjective
      chooseOrRunOneM lead do
        questionLabeled "takingOnWater.attach"
        causedBy attrs
        targets ships \s -> attachTreachery attrs.treacheryId s
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      ships <- filterM (\aid -> selectAny (shipObjective <> AllyWithId aid)) [aid | AllyTarget aid <- defenders]
      chooseOrRunOneM pid do
        questionLabeled "takingOnWater.attach"
        causedBy (CardIdTarget cid)
        targets ships $ attachShadowTreachery eid cid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> dealDamage attrs host 2
      pure t
    -- the characters are exhausted as the cost is paid
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      discard attrs
      pure t
    _ -> TakingOnWater <$> runMessage msg attrs
