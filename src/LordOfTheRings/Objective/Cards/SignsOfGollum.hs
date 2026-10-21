module LordOfTheRings.Objective.Cards.SignsOfGollum (signsOfGollum) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype SignsOfGollum = SignsOfGollum ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

signsOfGollum :: ObjectiveCard SignsOfGollum
signsOfGollum = objective SignsOfGollum Cards.signsOfGollum

-- | Response: After the players quest successfully, the players may claim Signs of Gollum
-- if it has no attached encounters. When claimed, attach Signs of Gollum to any hero
-- committed to the quest. (Counts as a Condition attachment with: "Forced: After attached
-- hero is damaged or leaves play, return this card to the top of the encounter deck.")
instance HasAbilities SignsOfGollum where
  getAbilities (SignsOfGollum a) = case a.placement of
    StagingArea ->
      [ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists CommittedHero)
          $ response
          $ QuestedSuccessfullyWindow After
      ]
    AttachedTo host -> [mkAbility a 2 $ forced $ DealtDamageWindow After host]
    _ -> []

instance HasModifiersFor SignsOfGollum where
  getModifiersFor (SignsOfGollum a) = case a.placement of
    AttachedTo _ -> modified a a [LeavesPlayToTopOfEncounterDeck]
    _ -> pure ()

instance RunMessage SignsOfGollum where
  runMessage msg o@(SignsOfGollum attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select CommittedHero
      chooseOneM pid do
        questionLabeled "signsOfGollum.attach"
        causedBy attrs
        targets heroes \h -> push $ ClaimObjective attrs.id (toTarget h)
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      push $ ReturnToTopOfEncounterDeck (toTarget attrs)
      pure o
    _ -> SignsOfGollum <$> runMessage msg attrs
