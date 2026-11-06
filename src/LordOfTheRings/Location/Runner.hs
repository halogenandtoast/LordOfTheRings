{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location.Runner (module X) where

import LordOfTheRings.Card (CardDef (..), toCard, toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource, modifiedQuestPoints)
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Modifier (ModifierType (GainVictory, NoProgressFromCardEffects, GainTrait, NoProgressFromPlayerCards, PhaseProgressLimitOnTrait, ProgressLimit, ProgressPerRoundLimit, ProgressPlacedOnTraitThisPhase, ProgressPlacedThisRound, ProgressReducedBy, ProgressReducedTo))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase, UntilEndOfRound))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

instance RunMessage LocationAttrs where
  runMessage msg attrs = case msg of
    PlaceLocation lid placement | lid == attrs.id -> pure $ attrs & placementL .~ placement
    -- tokens a player card would place or remove do not reach an immune location
    PlaceTokens source (isTarget attrs -> True) _ _ | source /= GameSource -> unlessImmune source
    RemoveTokens source (isTarget attrs -> True) _ _ | source /= GameSource -> unlessImmune source
    PlaceTokens source (isTarget attrs -> True) Progress n0 | n0 > 0 -> placeProgress source n0
    PlaceTokens source (isTarget attrs -> True) token n -> placeTokens source token n
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    -- a player card under it taken to its owner's hand or discard pile, as Truly Lost's
    PutInHand card -> pure $ attrs & underneathL %~ filter ((/= card.id) . (.id))
    PutInDiscard card -> pure $ attrs & underneathL %~ filter ((/= card.id) . (.id))
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    -- a location takes no more progress than it may this round, as The Hills of Emyn
    -- Muil's nightmare text limits the staging area's, and keeps count of what it took
    placeProgress source n0 = do
      mods <- getModifiers attrs
      gameMods <- getModifiers GameTarget
      -- reduced by so much first, as Deep Dark Passage reduces what a location in the
      -- staging area would take
      let n = max 0 $ foldr min (max 0 $ n0 - sum [k | ProgressReducedBy k <- mods]) ([k | ProgressReducedTo k <- mods] <> [k | ProgressLimit k <- mods])
          limits = [k | ProgressPerRoundLimit k <- mods]
          placed = sum [k | ProgressPlacedThisRound k <- mods]
          room = foldr (\k -> min (max 0 $ k - placed)) n limits
          -- what locations with a trait may still take together this phase, as Sulfur
          -- Cloud's limit on Underwater ones
          traits = cdCardTraits (toCardDef attrs) <> setFromList [t | GainTrait t <- mods]
          shared =
            [ (trait, max 0 $ k - sum [p | ProgressPlacedOnTraitThisPhase trait' p <- gameMods, trait' == trait])
            | PhaseProgressLimitOnTrait trait k <- gameMods
            , trait `member` traits
            ]
          room' = foldr (min . snd) room shared
          record = [CreateEffect $ Effect GameSource GameTarget [ProgressPlacedOnTraitThisPhase trait room'] UntilEndOfPhase | (trait, _) <- shared, room' > 0]
      if
        | n <= 0 -> pure attrs
        | null limits && null shared -> placeTokens source Progress n
        | room' <= 0 -> pure attrs
        | otherwise -> do
            pushAll
              $ [CreateEffect $ Effect GameSource (toTarget attrs) [ProgressPlacedThisRound room'] UntilEndOfRound | notNull limits]
              <> record
            placeTokens source Progress room'
    placeTokens source token n = do
      let attrs' = attrs & tokensL %~ addTokens token n
      questPoints <- modifiedQuestPoints attrs attrs'.questPoints
      gainsVictory <- any (\case GainVictory _ -> True; _ -> False) <$> getModifiers attrs
      -- who placed it, as Haunted Hall reads "a player places any amount of progress"
      when (token == Progress && n > 0) $ push $ CheckWindows [mkAfter $ ProgressPlacedOnLocationBy source attrs.id]
      -- a location with as much progress as its quest points is explored, but one on the
      -- Island Map stays in its place (Temple of the Deceived)
      when (token == Progress && questPoints > 0 && attrs'.progress >= questPoints && isNothing attrs.position)
        $ pushAll
          [ CheckWindows $ map mkWhen explored
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs) || gainsVictory
              then AddToVictoryDisplay (toTarget attrs)
              else Discard (toSource attrs) (toTarget attrs)
          , -- after it is explored, as Idraen answers; whether it was the active location,
            -- and its card, are told now, as it is gone by then
            CheckWindows $ map mkAfter (explored <> [ExploredCard (toCard attrs)])
          ]
      pure attrs'
    explored = Explored attrs.id : [ActiveLocationExplored attrs.id | attrs.placement == ActiveLocation]
    unlessImmune source = do
      mods <- getModifiers attrs
      let barred = case msg of
            PlaceTokens _ _ Progress _ -> NoProgressFromPlayerCards `elem` mods
            _ -> False
          -- no card places progress on a location that takes no more, as Broken Lands bars it
          noMore = case msg of
            PlaceTokens _ _ Progress _ -> ProgressLimit 0 `elem` mods
            _ -> False
      let effectsBarred = case msg of
            PlaceTokens _ _ Progress _ -> NoProgressFromCardEffects `elem` mods
            _ -> False
      immune <- (|| noMore || effectsBarred || (isPlayerCardSource source && barred)) <$> immuneToSource source attrs
      -- the message again, from a source the guard lets by; progress still told as the
      -- source's
      if
        | immune -> pure attrs
        | PlaceTokens _ _ Progress n <- msg, n > 0 -> placeProgress source n
        | otherwise -> runMessage (asGame msg) attrs
    asGame = \case
      PlaceTokens _ t token n -> PlaceTokens GameSource t token n
      RemoveTokens _ t token n -> RemoveTokens GameSource t token n
      m -> m
