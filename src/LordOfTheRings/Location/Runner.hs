{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location.Runner (module X) where

import LordOfTheRings.Card (CardDef (..), toCard, toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource, modifiedValue, questPointsOf)
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Modifier (ModifierType (GainTrait, NoProgressFromPlayerCards, PhaseProgressLimitOnTrait, ProgressLimit, ProgressPerRoundLimit, ProgressPlacedOnTraitThisPhase, ProgressPlacedThisRound, ProgressReducedTo))
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
    -- a location takes no more progress than it may this round, as The Hills of Emyn
    -- Muil's nightmare text limits the staging area's, and keeps count of what it took
    PlaceTokens source (isTarget attrs -> True) Progress n0 | n0 > 0 -> do
      mods <- getModifiers attrs
      gameMods <- getModifiers GameTarget
      let n = foldr min n0 [k | ProgressReducedTo k <- mods]
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
          record = [CreateEffect $ Effect source GameTarget [ProgressPlacedOnTraitThisPhase trait room'] UntilEndOfPhase | (trait, _) <- shared, room' > 0]
      if
        | null limits && null shared -> placeTokens Progress n
        | room' <= 0 -> pure attrs
        | otherwise -> do
            pushAll
              $ [CreateEffect $ Effect source (toTarget attrs) [ProgressPlacedThisRound room'] UntilEndOfRound | notNull limits]
              <> record
            placeTokens Progress room'
    PlaceTokens _ (isTarget attrs -> True) token n -> placeTokens token n
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    placeTokens token n = do
      let attrs' = attrs & tokensL %~ addTokens token n
      questPoints <- modifiedValue questPointsOf attrs attrs'.questPoints
      -- a location with as much progress as its quest points is explored, but one on the
      -- Island Map stays in its place (Temple of the Deceived)
      when (token == Progress && questPoints > 0 && attrs'.progress >= questPoints && isNothing attrs.position)
        $ pushAll
          [ CheckWindows $ map mkWhen explored
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs)
              then AddToVictoryDisplay (toTarget attrs)
              else Discard (toTarget attrs)
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
      immune <- (|| noMore || (isPlayerCardSource source && barred)) <$> immuneToSource source attrs
      -- the message again, from a source the guard lets by
      if immune then pure attrs else runMessage (asGame msg) attrs
    asGame = \case
      PlaceTokens _ t token n -> PlaceTokens GameSource t token n
      RemoveTokens _ t token n -> RemoveTokens GameSource t token n
      m -> m
