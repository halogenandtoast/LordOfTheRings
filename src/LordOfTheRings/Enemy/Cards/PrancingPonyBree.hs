module LordOfTheRings.Enemy.Cards.PrancingPonyBree (billFernyMurder, breeLandInformant) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (lookAtBottom)
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Card (cdShadowEffect, toCardDef)

newtype BillFernyMurder = BillFernyMurder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

billFernyMurder :: EnemyCard BillFernyMurder
billFernyMurder = enemy BillFernyMurder Cards.billFernyMurder

instance HasModifiersFor BillFernyMurder where
  getModifiersFor (BillFernyMurder a) = do
    modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]
    modified a GameTarget [FirstTreacheryEachRoundGainsDoomed 1]
    effects <- getsGame gameEffects
    treacheries <- getsGame (.entities.treacheries)
    let revealed = [tid | Effect (TreacherySource tid) _ mods _ <- effects, TreacheryRevealedThisRound `elem` mods]
        current = headMay [tid | Effect _ _ mods _ <- effects, RevealingTreachery tid <- mods, Map.member tid treacheries]
    for_ (lastMay revealed) $ \tid -> do
      modified a (TreacheryTarget tid) [WhenRevealedCannotBeCanceled]
      when (current == Just tid) $ modified a GameTarget [CannotCancelEncounterEffects]

instance RunMessage BillFernyMurder where
  runMessage msg e@(BillFernyMurder a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseOneM pid do
        questionLabeled "prancingPony.billShadow"
        causedBy a
        labeled "prancingPony.raiseThreat4" $ encounterRaisesThreat pid 4
        labeled "prancingPony.undefended" $ doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> e <$ makeUndefended pid eid
    _ -> BillFernyMurder <$> runMessage msg a

newtype BreeLandInformant = BreeLandInformant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

breeLandInformant :: EnemyCard BreeLandInformant
breeLandInformant = enemy BreeLandInformant Cards.breeLandInformant

instance HasAbilities BreeLandInformant where
  getAbilities (BreeLandInformant a) =
    [ anyPlayer $ restricted a 1
        (TargetHasModifier (toTarget a) AttackWasDealtBlankShadow <> TargetHasModifier GameTarget InvestigationDeckHasCards)
        $ ResponseAbility (EnemyAttacksYouWindow After $ EnemyWithId a.id) (Costs [RaiseThreatCost 1, SpendResourcesCost Neutral 1])
    ]

instance RunMessage BreeLandInformant where
  runMessage msg e@(BreeLandInformant a) = case msg of
    CheckWindows windows -> do
      when (any (\case Window When (EnemyAttacking eid _) -> eid == a.id; _ -> False) windows) $ push $ EndEffectsFrom $ toSource a
      when (any (not . cdShadowEffect . toCardDef) [card | (card, _, _) <- revealedShadowCards a windows]) $
        createEffect a a [AttackWasDealtBlankShadow] UntilEndOfPhase
      BreeLandInformant <$> runMessage msg a
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtBottom pid a
      discard a a
      pure e
    _ -> BreeLandInformant <$> runMessage msg a
