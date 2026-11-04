module LordOfTheRings.Objective.Cards.TheBlackSword (theBlackSword) where

import LordOfTheRings.Card (CardCode (..), EncounterCard (..), toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype TheBlackSword = TheBlackSword ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign, attached to Captain Sahír as The City of
-- Corsairs' campaign card says.
theBlackSword :: ObjectiveCard TheBlackSword
theBlackSword = objective TheBlackSword Cards.theBlackSword

resources :: ObjectiveAttrs -> Int
resources = countTokens Resource . objectiveTokens

-- | Immune to player card effects. Captain Sahír gets +2 attack and gains indestructible.
instance HasModifiersFor TheBlackSword where
  getModifiersFor (TheBlackSword a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    case a.placement of
      AttachedTo host@(EnemyTarget _) -> modified a host [AttackModifier 2, GainKeyword Indestructible]
      _ -> pure ()

{- | The Black Sword enters play with 3 resources on it: the campaign card that attaches it
places them, no window opening for an objective put into play attached.
The City of Corsairs' campaign card: Refresh Action: Exhaust a hero to move 1 resource from
The Black Sword to that hero's resource pool. If The Black Sword has no resources on it,
remove it from the game. (Group limit once per round.)
-}
instance HasAbilities TheBlackSword where
  getAbilities (TheBlackSword a) =
    [ limited (GroupLimit PerRound 1)
        $ anyPlayer
        $ restricted a 2 (DuringPhase RefreshPhase)
        $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    ]

firstAttack :: ModifierType
firstAttack = UsedThisRound (CardCode "black-sword-attack")

-- | Forced: After Captain Sahír attacks for the first time each round, he makes an attack
-- against each other player (in turn order).
instance RunMessage TheBlackSword where
  runMessage msg s@(TheBlackSword a) = case msg of
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      -- the hero exhausted to pay
      for_ [h | Window _ (Exhausted h) <- windows] \h -> placeResources a h 1
      removeTokens a a Resource 1
      when (resources a <= 1) $ removeFromGame a
      pure s
    CheckWindows ws | AttachedTo (EnemyTarget sahir) <- a.placement -> do
      marks <- getModifiers a
      for_ (take 1 [p | Window After (EnemyAttacking eid p) <- ws, eid == sahir, firstAttack `notElem` marks]) \p -> do
        roundModifier a a firstAttack
        others <- filter (/= p) <$> getPlayerOrder
        for_ others \o -> immediateAttack o sahir
      TheBlackSword <$> runMessage msg a
    Discard (isTarget a -> True) -> do
      s' <- TheBlackSword <$> runMessage msg a
      removeFromEncounterDiscard (MkEncounterCard a.cardId (toCardCode a))
      pure s'
    _ -> TheBlackSword <$> runMessage msg a
