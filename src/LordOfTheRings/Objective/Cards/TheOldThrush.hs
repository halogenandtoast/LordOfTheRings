module LordOfTheRings.Objective.Cards.TheOldThrush (theOldThrush) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype TheOldThrush = TheOldThrush ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldThrush :: ObjectiveCard TheOldThrush
theOldThrush = objective TheOldThrush Cards.theOldThrush

instance HasModifiersFor TheOldThrush where
  getModifiersFor (TheOldThrush a) = modifySelf a [LeavesPlayToEncounterDiscard]

instance HasAbilities TheOldThrush where
  getAbilities (TheOldThrush a) = case a.placement of
    StagingArea ->
      [ anyPlayer $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard)
          $ ActionAbility $ ExhaustHeroCost (HeroControlledBy ActivePlayer)
      ]
    AttachedTo host@(HeroTarget hid) ->
      [ anyPlayer $ restricted a 2 (exists (HeroWithId hid <> HeroControlledBy ActivePlayer) <> PlayerExists (ActivePlayer <> PlayerWithCardsInHand 1))
          $ response $ ExhaustedToAttackWindow After host smaug
      ]
    _ -> []
   where
    smaug = EnemyWithTitleContaining "Smaug"

instance RunMessage TheOldThrush where
  runMessage msg o@(TheOldThrush a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [hid | Window _ (Exhausted (HeroTarget hid)) <- windows] $ attachObjective a.id
      pure o
    UseCardAbility pid (isSource a -> True) 2 windows -> do
      for_ [eid | Window _ (ExhaustedToAttack _ eid) <- windows] \eid ->
        attackModifier a eid PendingDefenseReductionFromDiscard
      discardAnyFromHandWithResult a pid
      pure o
    DiscardedChosenFromHand (isSource a -> True) _ cards -> do
      effects <- getsGame gameEffects
      for_ [e | e <- effects, effectSource e == toSource a, PendingDefenseReductionFromDiscard `elem` effectModifiers e] \effect -> do
        for_ [eid | EnemyTarget eid <- [effectTarget effect]] \eid ->
          attackModifier a eid (DefenseModifier $ negate $ length cards)
        push $ EndEffect effect
      pure o
    PlaceObjective oid _ | oid == a.id, AttachedTo (HeroTarget _) <- a.placement -> do
      detached <- runMessage msg a
      discard detached detached
      pure $ TheOldThrush detached
    ClaimObjective oid host | oid == a.id, AttachedTo old <- a.placement, host /= old -> o <$ discard a a
    _ -> TheOldThrush <$> runMessage msg a
