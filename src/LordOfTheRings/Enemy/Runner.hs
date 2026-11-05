{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy.Runner (module X, placeDamageWithinRoundLimit, revealedShadowCards) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Card (Card (EncounterCard), EncounterCard)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.Types as X
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Modifier (ModifierType (DamageLimit, DamageLimitEachRound, DamageTakenThisRound, KeepUnresolvedShadowCards, MaxDamage))
import LordOfTheRings.Phase
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Question
import LordOfTheRings.Source (sourceTarget, toSource)
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (ShadowCardDealt, ShadowCardRevealed), mkAfter)

{- | The enemy's shadow cards turning faceup in these windows, still dealt to it, with the
defending player and characters: "When/If/After ... is dealt a shadow card with ..." is
checked here, once for each card, and never for one discarded before it turned faceup
(Nate French, ALeP 3.003; Caleb, ALeP 3.274 #3).
-}
revealedShadowCards :: EnemyAttrs -> [Window] -> [(EncounterCard, PlayerId, [Target])]
revealedShadowCards attrs ws =
  [(c, pid, defenders) | Window When (ShadowCardRevealed eid cid pid defenders) <- ws, eid == attrs.id, c <- attrs.shadowCards, c.id == cid]

instance RunMessage EnemyAttrs where
  runMessage msg attrs = case msg of
    PlaceEnemy eid placement | eid == attrs.id -> pure $ attrs & placementL .~ placement
    AttachShadowCard eid card | eid == attrs.id -> do
      for_ (placementController attrs.placement) \pid ->
        push $ CheckWindows [mkAfter $ ShadowCardDealt eid card.id pid]
      pure $ attrs & shadowCardsL %~ (<> [card])
    LookAtShadowCard pid source eid cid | eid == attrs.id -> do
      for_ (find ((== cid) . (.id)) attrs.shadowCards) \card ->
        push
          $ Ask pid
          $ QuestionLabel (ikey "label.shadowCard") (sourceTarget source) []
          $ SearchCards [EncounterCard card]
          $ ChooseOne [Label (ikey "label.continue") []]
      pure attrs
    RemoveShadowCard eid cid | eid == attrs.id -> pure $ attrs & shadowCardsL %~ filter ((/= cid) . (.id))
    RevealShadowCard eid cid | eid == attrs.id -> pure $ attrs & shadowRevealedL %~ (cid :) & shadowFaceUpL %~ filter (/= cid)
    TurnShadowCardFaceUp eid cid | eid == attrs.id -> pure $ attrs & shadowRevealedL %~ (cid :) & shadowFaceUpL %~ (cid :)
    -- the unresolved ones may stay, as The Clutches of Carn Dûm reads: those resolved, face
    -- up and no longer waiting to, go
    EndPhase CombatPhase | notNull attrs.shadowCards -> do
      keep <- elem KeepUnresolvedShadowCards <$> getModifiers GameTarget
      let resolved c = c.id `elem` attrs.enemyShadowRevealed && c.id `notElem` attrs.enemyShadowFaceUp
      if keep
        then do
          let (gone, kept) = partition resolved attrs.shadowCards
          pushWhen (notNull gone) $ AddToEncounterDiscard gone
          let stays = (`elem` map (.id) kept)
          pure $ attrs & shadowCardsL .~ kept & shadowRevealedL %~ filter stays & shadowFaceUpL %~ filter stays
        else discardShadows
    DiscardShadowCards eid | eid == attrs.id && notNull attrs.shadowCards -> discardShadows
    AttachFacedown (isTarget attrs -> True) card -> pure $ attrs & facedownL %~ (<> [card])
    -- the cards facedown under it go to their owners' discard piles as it leaves play
    Destroy _ (isTarget attrs -> True) -> leaving
    Discard _ (isTarget attrs -> True) -> leaving
    ReturnToTopOfEncounterDeck (isTarget attrs -> True) -> leaving
    ReturnToBottomOfEncounterDeck (isTarget attrs -> True) -> leaving
    ShuffleIntoEncounterDeck (isTarget attrs -> True) -> leaving
    RemoveFromGame (isTarget attrs -> True) -> leaving
    -- one that cannot take more than N damage each round, as Mumak: damage past what the
    -- round has left is lost, and what it has left is kept as a round effect
    -- one that cannot take more than N damage each round while another card says so, as A
    -- Haunt for Beasts, counts what it took this round against that card's limit
    PlaceTokens _ (isTarget attrs -> True) Damage n | n > 0 -> do
      mods <- getModifiers attrs
      let left = foldr min maxBound [k | DamageLimit k <- mods]
          -- one that cannot have more than N damage, as City Guard
          room = foldr min maxBound [max 0 (k - attrs.damage) | MaxDamage k <- mods]
          taken = sum [k | DamageTakenThisRound k <- mods]
          eachRound = foldr min maxBound [max 0 (k - taken) | DamageLimitEachRound k <- mods]
          placed = min room (min n (min left eachRound))
      when (left < maxBound)
        $ push
        $ CreateEffect
        $ Effect (toSource attrs) (toTarget attrs) [DamageLimit (left - placed)] UntilEndOfRound
      when (eachRound < maxBound && placed > 0)
        $ push
        $ CreateEffect
        $ Effect (toSource attrs) (toTarget attrs) [DamageTakenThisRound placed] UntilEndOfRound
      pure $ attrs & tokensL %~ addTokens Damage placed
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    leaving = do
      pushAll $ map PutInDiscard attrs.facedown
      pure $ attrs & facedownL .~ []
    discardShadows = do
      push $ AddToEncounterDiscard attrs.shadowCards
      pure $ attrs & shadowCardsL .~ [] & shadowRevealedL .~ [] & shadowFaceUpL .~ []

{- | Damage placed on an enemy that cannot take more than X damage each round, X counted
from the board, as Dagnir's: what it has taken this round is kept as a round effect, and X
is read as the damage is placed, so an X that rises during the round lets it take more.
Damage past what X leaves is lost.
-}
placeDamageWithinRoundLimit :: Int -> Int -> EnemyAttrs -> QueueT Message GameT EnemyAttrs
placeDamageWithinRoundLimit x n attrs = do
  taken <- sum . (\ms -> [k | DamageTakenThisRound k <- ms]) <$> getModifiers attrs
  let placed = max 0 (min n (x - taken))
  pushWhen (placed > 0)
    $ CreateEffect
    $ Effect (toSource attrs) (toTarget attrs) [DamageTakenThisRound placed] UntilEndOfRound
  pure $ attrs & tokensL %~ addTokens Damage placed
