module LordOfTheRings.Hero.Cards.FrodoBagginsTheBlackRiders (frodoBagginsTheBlackRiders) where

import LordOfTheRings.Helpers.RingBearer (exhaustTheOneRing, sagaOneRingReady)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype FrodoBagginsTheBlackRiders = FrodoBagginsTheBlackRiders HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheBlackRiders :: HeroCard FrodoBagginsTheBlackRiders
frodoBagginsTheBlackRiders = hero FrodoBagginsTheBlackRiders Cards.frodoBagginsTheBlackRiders

-- | Response: Spend 1 [fellowship] resource and exhaust The One Ring to cancel the effect of
-- an encounter card just revealed from the encounter deck. Shuffle that card back into the
-- encounter deck and reveal another encounter card. Answered before its keywords and When
-- Revealed effects resolve, as The Master Ring is.
instance HasAbilities FrodoBagginsTheBlackRiders where
  getAbilities (FrodoBagginsTheBlackRiders a) =
    [ restricted a 1 (EncounterEffectsCancelable <> sagaOneRingReady)
        $ ResponseAbility (EncounterCardRevealedWindow When) (SphereResourceCost a.owner Fellowship 1)
    ]

instance RunMessage FrodoBagginsTheBlackRiders where
  runMessage msg h@(FrodoBagginsTheBlackRiders attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      exhaustTheOneRing
      for_ (take 1 [t | Window _ (Revealed t) <- windows]) \t -> do
        cancelWhenRevealed t
        -- what it keeps in play, its surge and doomed, goes with it into the deck
        cancelMatching \case
          FinishRevelation tid -> TreacheryTarget tid == t
          ResolveSurge t' _ -> t' == t
          _ -> False
        shuffleIntoEncounterDeck t
        revealEncounterCardAfter t
      pure h
    _ -> FrodoBagginsTheBlackRiders <$> runMessage msg attrs
