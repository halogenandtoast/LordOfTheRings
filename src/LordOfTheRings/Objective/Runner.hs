{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Objective.Runner (module X) where

import LordOfTheRings.Card (cdCardType, cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasQueue (pushAll, pushWhen)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Keyword (Keyword (GuardedBy))
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (addToStagingFromDiscard, guardObjectiveWith)
import LordOfTheRings.Objective.Claim (claimObjective)
import LordOfTheRings.Objective.Types as X
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (EntersStagingArea, Exhausted), mkAfter)

instance RunMessage ObjectiveAttrs where
  runMessage msg attrs = case msg of
    ClaimObjective oid host | oid == attrs.objectiveId -> pure $ attrs & placementL .~ AttachedTo host
    PlaceObjective oid placement | oid == attrs.objectiveId -> do
      pushWhen (placement == StagingArea && attrs.placement /= StagingArea)
        $ CheckWindows [mkAfter $ EntersStagingArea (toTarget attrs)]
      pure $ attrs & placementL .~ placement & guardL .~ Nothing
    SetObjectiveGuard oid cid | oid == attrs.objectiveId -> pure $ attrs & guardL ?~ cid
    -- Guarded (X): the first card of its types discarded goes to the staging area, guarding
    -- it (MEC59 rules)
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let types = [t | GuardedBy ts <- toList (cdKeywords $ toCardDef attrs), t <- ts]
      for_ (find ((`elem` types) . cdCardType . toCardDef) cards) \card -> do
        addToStagingFromDiscard card
        guardObjectiveWith attrs.id card.id
      pure attrs
    ClaimFreeObjective oid | oid == attrs.objectiveId -> do
      lead <- getLead
      claimObjective lead attrs
      pure attrs
    Exhaust (isTarget attrs -> True) | not attrs.exhausted -> do
      pushAll $ doFrame msg (Exhausted $ toTarget attrs)
      pure attrs
    Do (Exhaust (isTarget attrs -> True)) -> pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
