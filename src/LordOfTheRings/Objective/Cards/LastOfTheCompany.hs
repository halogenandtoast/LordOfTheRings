module LordOfTheRings.Objective.Cards.LastOfTheCompany (lastOfTheCompany) where

import LordOfTheRings.Card.CardDef (cdCardCode)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (readyOneRing)
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Trait (Trait (Orc, RingBearer))

newtype LastOfTheCompany = LastOfTheCompany ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

lastOfTheCompany :: ObjectiveCard LastOfTheCompany
lastOfTheCompany = objective LastOfTheCompany Cards.lastOfTheCompany

-- | Attached hero gains the Ring-bearer trait and the Fellowship sphere. You cannot lose
-- control of the first player token.
instance HasModifiersFor LastOfTheCompany where
  getModifiersFor (LastOfTheCompany a) = for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
    modified a (HeroTarget hid) [GainTrait RingBearer, GainResourceIcon Fellowship]
    you <- (.owner) <$> getHero hid
    modified a (PlayerTarget you) [CannotLoseFirstPlayerToken]

{- | Action: Exhaust The One Ring and raise your threat by 1 to choose an Orc enemy. Until
the end of the phase, that enemy gets -X threat, where X is the attached hero's willpower.

The One Ring is no card of this one's to name as a cost: its criteria check it is ready,
and it is exhausted as the ability resolves, as a Master card's is ('payTheOneRing').
-}
instance HasAbilities LastOfTheCompany where
  getAbilities (LastOfTheCompany a) =
    [ restricted a 1 (exists (HeroWithReadyAttachment (cdCardCode Cards.theOneRingTheMountainOfFire)) <> exists (EnemyWithTrait Orc))
        $ ActionAbility (RaiseThreatCost 1)
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage LastOfTheCompany where
  runMessage msg o@(LastOfTheCompany attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      traverse_ (exhaust . ObjectiveTarget) =<< readyOneRing
      for_ [hid | AttachedTo (HeroTarget hid) <- [attrs.placement]] \hid -> do
        x <- getWillpower (HeroTarget hid)
        enemies <- select (EnemyWithTrait Orc)
        chooseOrRunOneM pid do
          questionLabeled "lastOfTheCompany.enemy"
          causedBy attrs
          targets enemies \e -> phaseModifier attrs e (ThreatModifier (negate x))
      pure o
    _ -> LastOfTheCompany <$> runMessage msg attrs
