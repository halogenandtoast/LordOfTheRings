module LordOfTheRings.Objective.Cards.BlackKeyBurden (blackKeyBurden, blackKeyBurdenUpgraded) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode, toCardDef)
import LordOfTheRings.Card.CardDef (cdCardTraits)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Upgraded))

newtype BlackKeyBurden = BlackKeyBurden ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The burden side of Black Key, attached to an enemy as the campaign card's setup says.
blackKeyBurden :: ObjectiveCard BlackKeyBurden
blackKeyBurden = objective BlackKeyBurden Cards.blackKeyBurden

blackKeyBurdenUpgraded :: ObjectiveCard BlackKeyBurden
blackKeyBurdenUpgraded = objective BlackKeyBurden Cards.blackKeyBurdenUpgraded

-- | Attached enemy gets +1 threat, +1 attack, +1 defense, and gains immune to player card
-- effects.
instance HasModifiersFor BlackKeyBurden where
  getModifiersFor (BlackKeyBurden a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]
    _ -> pure ()

-- | Forced: At the end of the quest phase, if Captain Sahír is in play, place 1 resource on
-- him for each player in the game. (Not on its Upgraded side.)
instance HasAbilities BlackKeyBurden where
  getAbilities (BlackKeyBurden a) =
    [ mkAbility a 1 $ forced $ PhaseEndsWindow After QuestPhase
    | Upgraded `notMember` cdCardTraits (toCardDef a)
    ]

-- | When Black Key leaves play, remove it from the game.
instance RunMessage BlackKeyBurden where
  runMessage msg s@(BlackKeyBurden a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      sahirs <- select (EnemyWithCardCode $ toCardCode Enemies.captainSahir)
      allies <- select (AllyWithCardCode "11077")
      for_ sahirs \e -> placeResources a e players
      for_ allies \al -> placeResources a al players
      pure s
    Discard _ (isTarget a -> True) -> do
      s' <- BlackKeyBurden <$> runMessage msg a
      removeFromEncounterDiscard (MkEncounterCard a.cardId (toCardCode a))
      pure s'
    _ -> BlackKeyBurden <$> runMessage msg a
